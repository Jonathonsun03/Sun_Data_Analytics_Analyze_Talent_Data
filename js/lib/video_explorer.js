(function (global) {
  "use strict";

  function ensureIndexes(data) {
    if (data.__edgesByVideo) return data.__edgesByVideo;
    const edgesByVideo = Array.from({ length: data.videos.length }, () => []);
    (data.edges || []).forEach((edge) => {
      edgesByVideo[Number(edge[0])].push([
        Number(edge[1]),
        Number(edge[2])
      ]);
    });
    Object.defineProperty(data, "__edgesByVideo", {
      value: edgesByVideo,
      enumerable: false
    });
    return edgesByVideo;
  }

  function initialIds(data, options) {
    const requested = new Set((options.initialVideoIds || []).map(String));
    if (requested.size > 0) {
      return data.videos.filter((video) => requested.has(String(video.id))).map((video) => video.id);
    }
    return data.videos.slice(0, Number(options.initialVideoCount) || 12).map((video) => video.id);
  }

  function createCatalogPicker(videos, startingIds, options, onChange) {
    const settings = Object.assign({ maxSelected: 40, summaryPrefix: "Videos" }, options || {});
    const initial = new Set(startingIds.map(String));
    const selected = new Set(startingIds.map(String));
    const picker = document.createElement("details");
    picker.className = "sd-video-picker sd-video-picker--catalog";
    const summary = document.createElement("summary");
    summary.className = "sd-video-picker__summary";
    picker.append(summary);
    const panel = document.createElement("div");
    panel.className = "sd-video-picker__panel";
    const search = document.createElement("input");
    search.className = "sd-video-picker__search";
    search.type = "search";
    search.placeholder = `Search all ${videos.length.toLocaleString()} video titles`;
    search.setAttribute("aria-label", search.placeholder);
    const actions = document.createElement("div");
    actions.className = "sd-video-picker__actions";
    const initialButton = document.createElement("button");
    initialButton.type = "button";
    initialButton.textContent = "Initial selection";
    const noneButton = document.createElement("button");
    noneButton.type = "button";
    noneButton.textContent = "None";
    actions.append(initialButton, noneButton);
    const notice = document.createElement("div");
    notice.className = "sd-video-picker__notice";
    notice.textContent = `Choose up to ${settings.maxSelected} videos at once.`;
    const list = document.createElement("div");
    list.className = "sd-video-picker__options";
    panel.append(search, actions, notice, list);
    picker.append(panel);

    const rows = videos.map((video) => {
      const row = document.createElement("label");
      row.className = "sd-video-picker__option";
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.value = String(video.id);
      checkbox.checked = selected.has(String(video.id));
      const label = document.createElement("span");
      label.textContent = video.optionLabel || video.label;
      row.append(checkbox, label);
      list.append(row);
      checkbox.addEventListener("change", function () {
        if (checkbox.checked && selected.size >= settings.maxSelected) {
          checkbox.checked = false;
          notice.textContent = `Selection limit reached (${settings.maxSelected}). Clear another video first.`;
          return;
        }
        if (checkbox.checked) selected.add(checkbox.value);
        else selected.delete(checkbox.value);
        notice.textContent = `Choose up to ${settings.maxSelected} videos at once.`;
        updateSummary();
        onChange(new Set(selected));
      });
      return {
        checkbox,
        row,
        searchText: String(video.optionLabel || video.label).toLocaleLowerCase()
      };
    });

    function updateSummary() {
      summary.textContent = `${settings.summaryPrefix}: ${selected.size} selected · ${videos.length.toLocaleString()} available`;
    }

    function applySet(values) {
      selected.clear();
      values.forEach((value) => selected.add(String(value)));
      rows.forEach((row) => {
        row.checkbox.checked = selected.has(row.checkbox.value);
      });
      updateSummary();
      onChange(new Set(selected));
    }

    search.addEventListener("input", function () {
      const query = search.value.trim().toLocaleLowerCase();
      rows.forEach((row) => {
        row.row.hidden = Boolean(query) && !row.searchText.includes(query);
      });
    });
    initialButton.addEventListener("click", () => applySet(initial));
    noneButton.addEventListener("click", () => applySet([]));
    updateSummary();
    return picker;
  }

  function buildNetwork(data, selectedIds, options) {
    const edgesByVideo = ensureIndexes(data);
    const videoIndexById = new Map(data.videos.map((video, index) => [String(video.id), index]));
    const selectedIndexes = Array.from(selectedIds)
      .map((id) => videoIndexById.get(String(id)))
      .filter((index) => index !== undefined);
    const userMessages = new Map();
    selectedIndexes.forEach((videoIndex) => {
      edgesByVideo[videoIndex].forEach(([userIndex, messages]) => {
        userMessages.set(userIndex, (userMessages.get(userIndex) || 0) + messages);
      });
    });
    const chosenUsers = Array.from(userMessages.entries())
      .sort((a, b) => b[1] - a[1] || a[0] - b[0])
      .slice(0, Number(options.maxUsers) || 100);
    const chosenUserIndexes = new Set(chosenUsers.map(([index]) => index));
    const labelCount = Number(options.labelCount) || 18;
    const nodes = chosenUsers.map(([userIndex, messages], rank) => ({
      id: `user:${data.users[userIndex].id}`,
      type: "User",
      label: data.users[userIndex].label,
      weight: messages,
      showLabel: rank < labelCount
    }));
    const links = [];
    const videoWeights = new Map();
    selectedIndexes.forEach((videoIndex) => {
      const video = data.videos[videoIndex];
      let displayedMessages = 0;
      edgesByVideo[videoIndex].forEach(([userIndex, messages]) => {
        if (!chosenUserIndexes.has(userIndex)) return;
        displayedMessages += messages;
        links.push({
          source: `user:${data.users[userIndex].id}`,
          target: `video:${video.id}`,
          type: "engagement",
          weight: messages
        });
      });
      videoWeights.set(videoIndex, displayedMessages);
      nodes.push({
        id: `video:${video.id}`,
        type: "Video",
        label: video.label,
        weight: displayedMessages || video.messages,
        showLabel: true
      });
    });
    const streamers = new Map();
    selectedIndexes.forEach((videoIndex) => {
      const video = data.videos[videoIndex];
      const streamerId = String(video.streamerId);
      const current = streamers.get(streamerId) || {
        id: streamerId,
        label: video.talentCode,
        weight: 0
      };
      current.weight += videoWeights.get(videoIndex) || 0;
      streamers.set(streamerId, current);
      links.push({
        source: `video:${video.id}`,
        target: `streamer:${streamerId}`,
        type: "ownership",
        weight: videoWeights.get(videoIndex) || video.messages
      });
    });
    streamers.forEach((streamer) => {
      nodes.push({
        id: `streamer:${streamer.id}`,
        type: "Streamer",
        label: streamer.label,
        weight: streamer.weight,
        showLabel: true
      });
    });
    return { nodes, links, displayedUsers: chosenUsers.length };
  }

  function renderNetwork(element, data, options) {
    const settings = Object.assign({
      height: 760,
      initialVideoCount: 24,
      maxSelected: 40,
      maxUsers: 100,
      labelCount: 18
    }, options || {});
    const startingIds = initialIds(data, settings);
    element.replaceChildren();
    const shell = document.createElement("div");
    shell.className = "sd-video-explorer";
    const controls = document.createElement("div");
    controls.className = "sd-video-explorer__controls";
    const status = document.createElement("span");
    status.className = "sd-video-explorer__status";
    const chart = document.createElement("div");
    shell.append(controls, chart);
    element.append(shell);

    function update(selected) {
      const network = buildNetwork(data, selected, settings);
      status.textContent = `${selected.size} video${selected.size === 1 ? "" : "s"} selected · ${network.displayedUsers.toLocaleString()} chatters displayed`;
      global.SunDataNetwork.renderViewerActivityNetwork(chart, network, {
        height: settings.height,
        showVideoPicker: false
      });
    }

    const picker = createCatalogPicker(data.videos, startingIds, {
      maxSelected: settings.maxSelected,
      summaryPrefix: "Videos"
    }, update);
    controls.append(picker, status);
    update(new Set(startingIds.map(String)));
  }

  function buildOverlap(data, selectedIds) {
    const edgesByVideo = ensureIndexes(data);
    const videoIndexById = new Map(data.videos.map((video, index) => [String(video.id), index]));
    const videos = Array.from(selectedIds)
      .map((id) => videoIndexById.get(String(id)))
      .filter((index) => index !== undefined)
      .map((index, order) => Object.assign({}, data.videos[index], { index, order: order + 1 }));
    const memberSets = new Map(videos.map((video) => [
      String(video.id),
      new Set(edgesByVideo[video.index].map(([userIndex]) => userIndex))
    ]));
    const cells = [];
    videos.forEach((row) => {
      videos.forEach((column) => {
        const rowMembers = memberSets.get(String(row.id));
        const columnMembers = memberSets.get(String(column.id));
        const smaller = rowMembers.size <= columnMembers.size ? rowMembers : columnMembers;
        const larger = smaller === rowMembers ? columnMembers : rowMembers;
        let shared = 0;
        smaller.forEach((userIndex) => {
          if (larger.has(userIndex)) shared += 1;
        });
        const union = rowMembers.size + columnMembers.size - shared;
        cells.push({
          row_id: row.id,
          column_id: column.id,
          shared_chatters: shared,
          union_chatters: union,
          similarity: union > 0 ? shared / union : 0
        });
      });
    });
    return { videos, cells };
  }

  function renderOverlap(element, data, options) {
    const settings = Object.assign({
      height: 720,
      initialVideoCount: 16,
      maxSelected: 30
    }, options || {});
    const startingIds = initialIds(data, settings);
    element.replaceChildren();
    const shell = document.createElement("div");
    shell.className = "sd-video-explorer";
    const controls = document.createElement("div");
    controls.className = "sd-video-explorer__controls";
    const status = document.createElement("span");
    status.className = "sd-video-explorer__status";
    const chart = document.createElement("div");
    shell.append(controls, chart);
    element.append(shell);

    function update(selected) {
      status.textContent = `${selected.size} video${selected.size === 1 ? "" : "s"} compared`;
      if (selected.size === 0) {
        chart.replaceChildren();
        const empty = document.createElement("p");
        empty.className = "sd-video-explorer__empty";
        empty.textContent = "Choose at least one video to build the overlap matrix.";
        chart.append(empty);
        return;
      }
      global.SunDataCommunity.renderOverlap(chart, buildOverlap(data, selected), {
        height: settings.height,
        showVideoPicker: false
      });
    }

    const picker = createCatalogPicker(data.videos, startingIds, {
      maxSelected: settings.maxSelected,
      summaryPrefix: "Compare"
    }, update);
    controls.append(picker, status);
    update(new Set(startingIds.map(String)));
  }

  global.SunDataVideoExplorer = Object.assign(global.SunDataVideoExplorer || {}, {
    renderNetwork,
    renderOverlap
  });
}(window));

