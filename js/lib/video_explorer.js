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
    const settings = Object.assign({
      maxSelected: 40,
      summaryPrefix: "Videos",
      filterData: { topics: [], keywords: [] }
    }, options || {});
    const initial = new Set(startingIds.map(String));
    const selected = new Set(startingIds.map(String));
    const videoIds = new Set(videos.map((video) => String(video.id)));
    const matching = new Set();
    const selectedTopics = new Set();
    const selectedKeywords = new Set();
    let dateMode = "all";
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

    const filterBar = document.createElement("div");
    filterBar.className = "sd-video-picker__filters";
    const dateLabel = document.createElement("label");
    dateLabel.className = "sd-video-picker__date-filter";
    const dateText = document.createElement("span");
    dateText.textContent = "Date";
    const dateSelect = document.createElement("select");
    dateSelect.setAttribute("aria-label", "Filter streams by date");
    [
      ["all", "Any date"],
      ["30", "Last 30 days"],
      ["90", "Last 90 days"],
      ["this-year", "This year"],
      ["previous-year", "Previous year"],
      ["custom", "Custom range"]
    ].forEach(([value, label]) => {
      const option = document.createElement("option");
      option.value = value;
      option.textContent = label;
      dateSelect.append(option);
    });
    dateLabel.append(dateText, dateSelect);
    const customDates = document.createElement("div");
    customDates.className = "sd-video-picker__custom-dates";
    customDates.hidden = true;
    const startDate = document.createElement("input");
    startDate.type = "date";
    startDate.setAttribute("aria-label", "Streams starting on or after");
    const endDate = document.createElement("input");
    endDate.type = "date";
    endDate.setAttribute("aria-label", "Streams starting on or before");
    customDates.append(startDate, endDate);

    function valuesByVideo(sourceRows) {
      const result = new Map();
      (sourceRows || []).forEach((item) => {
        const videoId = String(item.videoId);
        if (!videoIds.has(videoId)) return;
        if (!result.has(videoId)) result.set(videoId, new Set());
        result.get(videoId).add(String(item.label));
      });
      return result;
    }
    const filterData = settings.filterData || { topics: [], keywords: [] };
    const topicsByVideo = valuesByVideo(filterData.topics);
    const keywordsByVideo = valuesByVideo(filterData.keywords);

    function facetValues(index, includeUnclassified) {
      const counts = new Map();
      index.forEach((values) => values.forEach((value) => counts.set(value, (counts.get(value) || 0) + 1)));
      const result = Array.from(counts, ([value, count]) => ({ value, label: value, count }))
        .sort((left, right) => left.label.localeCompare(right.label));
      if (includeUnclassified) {
        const count = videos.filter((video) => !(index.get(String(video.id))?.size)).length;
        if (count > 0) result.unshift({ value: "__unclassified__", label: "Unclassified", count });
      }
      return result;
    }

    function createFacetFilter(labelText, values, selectedValues, searchable) {
      const details = document.createElement("details");
      details.className = "sd-video-filter";
      const facetSummary = document.createElement("summary");
      facetSummary.textContent = labelText;
      details.append(facetSummary);
      const facetPanel = document.createElement("div");
      facetPanel.className = "sd-video-filter__panel";
      let facetSearch = null;
      if (searchable) {
        facetSearch = document.createElement("input");
        facetSearch.type = "search";
        facetSearch.className = "sd-video-filter__search";
        facetSearch.placeholder = `Search ${labelText.toLocaleLowerCase()}`;
        facetSearch.setAttribute("aria-label", facetSearch.placeholder);
        facetPanel.append(facetSearch);
      }
      const facetOptions = document.createElement("div");
      facetOptions.className = "sd-video-filter__options";
      const checkboxes = new Map();
      const facetRows = values.map((value) => {
        const row = document.createElement("label");
        row.className = "sd-video-filter__option";
        const checkbox = document.createElement("input");
        checkbox.type = "checkbox";
        checkbox.value = value.value;
        const text = document.createElement("span");
        text.textContent = `${value.label} (${value.count.toLocaleString()})`;
        checkbox.addEventListener("change", () => {
          if (checkbox.checked) selectedValues.add(checkbox.value);
          else selectedValues.delete(checkbox.value);
          applyFilters();
        });
        row.append(checkbox, text);
        facetOptions.append(row);
        checkboxes.set(value.value, checkbox);
        return { row, searchText: value.label.toLocaleLowerCase() };
      });
      if (facetSearch) {
        facetSearch.addEventListener("input", () => {
          const query = facetSearch.value.trim().toLocaleLowerCase();
          facetRows.forEach((row) => {
            row.row.hidden = Boolean(query) && !row.searchText.includes(query);
          });
        });
      }
      facetPanel.append(facetOptions);
      details.append(facetPanel);
      filterBar.append(details);
      details.addEventListener("toggle", () => {
        if (!details.open) return;
        filterBar.querySelectorAll("details.sd-video-filter").forEach((other) => {
          if (other !== details) other.open = false;
        });
      });
      return { summary: facetSummary, checkboxes, labelText };
    }

    filterBar.append(dateLabel, customDates);
    const topicFilter = createFacetFilter("Genre", facetValues(topicsByVideo, true), selectedTopics, false);
    const keywordFilter = createFacetFilter("Keyword", facetValues(keywordsByVideo, false), selectedKeywords, true);

    const chips = document.createElement("div");
    chips.className = "sd-video-picker__chips";
    const matchStatus = document.createElement("div");
    matchStatus.className = "sd-video-picker__match-status";
    const actions = document.createElement("div");
    actions.className = "sd-video-picker__actions";
    const initialButton = document.createElement("button");
    initialButton.type = "button";
    initialButton.textContent = "Initial selection";
    const noneButton = document.createElement("button");
    noneButton.type = "button";
    noneButton.textContent = "None";
    const selectMatchingButton = document.createElement("button");
    selectMatchingButton.type = "button";
    selectMatchingButton.textContent = "Select matching";
    const deselectMatchingButton = document.createElement("button");
    deselectMatchingButton.type = "button";
    deselectMatchingButton.textContent = "Deselect matching";
    actions.append(initialButton, noneButton, selectMatchingButton, deselectMatchingButton);
    const notice = document.createElement("div");
    notice.className = "sd-video-picker__notice";
    notice.textContent = `Choose up to ${settings.maxSelected} videos at once.`;
    const list = document.createElement("div");
    list.className = "sd-video-picker__options";
    panel.append(search, filterBar, chips, matchStatus, actions, notice, list);
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
        video,
        searchText: String(video.optionLabel || video.label).toLocaleLowerCase()
      };
    });

    function updateSummary() {
      summary.textContent = `${settings.summaryPrefix}: ${selected.size} selected · ${videos.length.toLocaleString()} available`;
      const matchingSelected = Array.from(matching).filter((id) => selected.has(id)).length;
      matchStatus.textContent = `${matching.size.toLocaleString()} matching · ${matchingSelected.toLocaleString()} matching selected · ${selected.size.toLocaleString()} selected total`;
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

    function dateMatches(video) {
      if (dateMode === "all") return true;
      const value = new Date(video.streamAt);
      if (!Number.isFinite(value.getTime())) return false;
      const now = new Date();
      if (dateMode === "30" || dateMode === "90") {
        const days = Number(dateMode);
        return value.getTime() >= now.getTime() - days * 24 * 60 * 60 * 1000;
      }
      if (dateMode === "this-year") return value.getUTCFullYear() === now.getUTCFullYear();
      if (dateMode === "previous-year") return value.getUTCFullYear() === now.getUTCFullYear() - 1;
      const isoDate = value.toISOString().slice(0, 10);
      return (!startDate.value || isoDate >= startDate.value) && (!endDate.value || isoDate <= endDate.value);
    }

    function facetMatches(video, chosen, index) {
      if (chosen.size === 0) return true;
      const values = index.get(String(video.id)) || new Set();
      return Array.from(chosen).some((value) => value === "__unclassified__" ? values.size === 0 : values.has(value));
    }

    function addChip(text, onRemove) {
      const chip = document.createElement("button");
      chip.type = "button";
      chip.className = "sd-video-picker__chip";
      chip.textContent = `${text} ×`;
      chip.setAttribute("aria-label", `Remove filter ${text}`);
      chip.addEventListener("click", onRemove);
      chips.append(chip);
    }

    function renderChips() {
      chips.replaceChildren();
      const query = search.value.trim();
      if (query) addChip(`Title: ${query}`, () => {
        search.value = "";
        applyFilters();
      });
      if (dateMode !== "all") {
        const label = dateMode === "custom"
          ? `Date: ${startDate.value || "any"}–${endDate.value || "any"}`
          : `Date: ${dateSelect.options[dateSelect.selectedIndex].textContent}`;
        addChip(label, () => {
          dateMode = "all";
          dateSelect.value = "all";
          customDates.hidden = true;
          applyFilters();
        });
      }
      selectedTopics.forEach((value) => addChip(`Genre: ${value === "__unclassified__" ? "Unclassified" : value}`, () => {
        selectedTopics.delete(value);
        topicFilter.checkboxes.get(value).checked = false;
        applyFilters();
      }));
      selectedKeywords.forEach((value) => addChip(`Keyword: ${value}`, () => {
        selectedKeywords.delete(value);
        keywordFilter.checkboxes.get(value).checked = false;
        applyFilters();
      }));
      if (chips.childElementCount > 0) {
        const clear = document.createElement("button");
        clear.type = "button";
        clear.className = "sd-video-picker__clear-filters";
        clear.textContent = "Clear filters";
        clear.addEventListener("click", clearFilters);
        chips.append(clear);
      }
    }

    function applyFilters() {
      const query = search.value.trim().toLocaleLowerCase();
      matching.clear();
      rows.forEach((row) => {
        const matches = (!query || row.searchText.includes(query)) &&
          dateMatches(row.video) &&
          facetMatches(row.video, selectedTopics, topicsByVideo) &&
          facetMatches(row.video, selectedKeywords, keywordsByVideo);
        row.row.hidden = !matches;
        if (matches) matching.add(String(row.video.id));
      });
      topicFilter.summary.textContent = selectedTopics.size > 0 ? `Genre (${selectedTopics.size})` : "Genre";
      keywordFilter.summary.textContent = selectedKeywords.size > 0 ? `Keyword (${selectedKeywords.size})` : "Keyword";
      renderChips();
      updateSummary();
    }

    function clearFilters() {
      search.value = "";
      dateMode = "all";
      dateSelect.value = "all";
      startDate.value = "";
      endDate.value = "";
      customDates.hidden = true;
      selectedTopics.clear();
      selectedKeywords.clear();
      topicFilter.checkboxes.forEach((checkbox) => { checkbox.checked = false; });
      keywordFilter.checkboxes.forEach((checkbox) => { checkbox.checked = false; });
      applyFilters();
    }

    search.addEventListener("input", function () {
      applyFilters();
    });
    dateSelect.addEventListener("change", () => {
      dateMode = dateSelect.value;
      customDates.hidden = dateMode !== "custom";
      applyFilters();
    });
    startDate.addEventListener("change", applyFilters);
    endDate.addEventListener("change", applyFilters);
    initialButton.addEventListener("click", () => applySet(initial));
    noneButton.addEventListener("click", () => applySet([]));
    selectMatchingButton.addEventListener("click", () => {
      let added = 0;
      let candidates = 0;
      matching.forEach((value) => {
        if (!selected.has(value)) candidates += 1;
        if (!selected.has(value) && selected.size < settings.maxSelected) {
          selected.add(value);
          added += 1;
        }
      });
      rows.forEach((row) => { row.checkbox.checked = selected.has(row.checkbox.value); });
      notice.textContent = candidates > added
        ? `Selection limit reached (${settings.maxSelected}). ${added} matching stream${added === 1 ? " was" : "s were"} added; ${candidates - added} could not be added.`
        : `${added} matching stream${added === 1 ? " was" : "s were"} added.`;
      updateSummary();
      onChange(new Set(selected));
    });
    deselectMatchingButton.addEventListener("click", () => {
      matching.forEach((value) => selected.delete(value));
      rows.forEach((row) => { row.checkbox.checked = selected.has(row.checkbox.value); });
      notice.textContent = "Matching streams were removed from the selection.";
      updateSummary();
      onChange(new Set(selected));
    });
    applyFilters();
    return picker;
  }

  function createSelectControl(labelText, choices, value, onChange) {
    const label = document.createElement("label");
    label.className = "sd-video-explorer__select-label";
    const text = document.createElement("span");
    text.textContent = labelText;
    const select = document.createElement("select");
    select.className = "sd-video-explorer__select";
    select.setAttribute("aria-label", labelText);
    choices.forEach((choice) => {
      const option = document.createElement("option");
      option.value = choice.value;
      option.textContent = choice.label;
      option.selected = choice.value === value;
      select.append(option);
    });
    select.addEventListener("change", () => onChange(select.value));
    label.append(text, select);
    return label;
  }

  function formatCount(value) {
    if (value === null || value === undefined || value === "") return "Unavailable";
    const number = Number(value);
    return Number.isFinite(number) ? number.toLocaleString() : "Unavailable";
  }

  function formatPercent(value, digits) {
    if (value === null || value === undefined || value === "") return "Unavailable";
    const number = Number(value);
    return Number.isFinite(number) ? `${number.toFixed(digits)}%` : "Unavailable";
  }

  function buildNetwork(data, selectedIds, options) {
    const edgesByVideo = ensureIndexes(data);
    const videoIndexById = new Map(data.videos.map((video, index) => [String(video.id), index]));
    const selectedIndexes = Array.from(selectedIds)
      .map((id) => videoIndexById.get(String(id)))
      .filter((index) => index !== undefined);
    const userActivity = new Map();
    selectedIndexes.forEach((videoIndex) => {
      edgesByVideo[videoIndex].forEach(([userIndex, messages]) => {
        const current = userActivity.get(userIndex) || { messages: 0, attendance: 0 };
        current.messages += messages;
        current.attendance += 1;
        userActivity.set(userIndex, current);
      });
    });
    const engagementField = options.engagementMode === "attendance" ? "attendance" : "messages";
    const chosenUsers = Array.from(userActivity.entries())
      .sort((a, b) => b[1][engagementField] - a[1][engagementField] || b[1].messages - a[1].messages || a[0] - b[0])
      .slice(0, Number(options.maxUsers) || 100);
    const chosenUserIndexes = new Set(chosenUsers.map(([index]) => index));
    const labelCount = Number(options.labelCount) || 18;
    const nodes = chosenUsers.map(([userIndex, activity], rank) => ({
      id: `user:${data.users[userIndex].id}`,
      type: "User",
      label: data.users[userIndex].label,
      weight: activity[engagementField],
      showLabel: rank < labelCount,
      tooltip: `Chatter: ${data.users[userIndex].label} · Messages sent: ${formatCount(activity.messages)} · Streams attended: ${formatCount(activity.attendance)}`
    }));
    const links = [];
    const videoWeights = new Map();
    selectedIndexes.forEach((videoIndex) => {
      const video = data.videos[videoIndex];
      let displayedMessages = 0;
      let displayedAttendance = 0;
      edgesByVideo[videoIndex].forEach(([userIndex, messages]) => {
        if (!chosenUserIndexes.has(userIndex)) return;
        displayedMessages += messages;
        displayedAttendance += 1;
        const chatter = data.users[userIndex];
        links.push({
          source: `user:${data.users[userIndex].id}`,
          target: `video:${video.id}`,
          type: "engagement",
          weight: engagementField === "attendance" ? 1 : messages,
          tooltip: `Chatter: ${chatter.label} · Video: ${video.label} · Messages sent: ${formatCount(messages)} · Attendance: 1`
        });
      });
      videoWeights.set(videoIndex, engagementField === "attendance" ? displayedAttendance : displayedMessages);
      nodes.push({
        id: `video:${video.id}`,
        type: "Video",
        label: video.label,
        weight: videoWeights.get(videoIndex) || 1,
        showLabel: true,
        tooltip: `Video: ${video.label} · Messages sent: ${formatCount(video.messages)} · Chatters: ${formatCount(video.chatters)}`
      });
    });

    const classificationType = options.classificationMode === "keywords" ? "Keyword" : "Topic";
    const classificationRows = data.classifications?.[options.classificationMode] || [];
    const selectedVideoIds = new Set(selectedIndexes.map((index) => String(data.videos[index].id)));
    const classificationNodes = new Map();
    classificationRows.forEach((row) => {
      if (!selectedVideoIds.has(String(row.videoId))) return;
      const nodeId = `${classificationType.toLocaleLowerCase()}:${row.label}`;
      if (!classificationNodes.has(nodeId)) {
        classificationNodes.set(nodeId, {
          id: nodeId,
          type: classificationType,
          label: row.label,
          weight: 0,
          showLabel: true,
          tooltip: `${classificationType}: ${row.label} · Total views: ${formatCount(row.classificationViews)}`
        });
      }
      classificationNodes.get(nodeId).weight += 1;
      links.push({
        source: `video:${row.videoId}`,
        target: nodeId,
        type: "classification",
        weight: Number(row.contributionPercentage) || 0,
        tooltip: `Video title: ${row.videoTitle} · ${classificationType}: ${row.label} · Video views: ${formatCount(row.videoViews)} · Total views for ${classificationType.toLocaleLowerCase()}: ${formatCount(row.classificationViews)} · Contribution percentage: ${formatPercent(row.contributionPercentage, 2)} · Classification confidence: ${formatPercent(row.confidence === null || row.confidence === undefined ? null : Number(row.confidence) * 100, 0)}`
      });
    });
    classificationNodes.forEach((classification) => {
      nodes.push({
        id: classification.id,
        type: classification.type,
        label: classification.label,
        weight: classification.weight,
        showLabel: classification.showLabel,
        tooltip: classification.tooltip
      });
    });
    const engagementLegend = engagementField === "attendance" ? "attendance (1 per chatter–video edge)" : "messages sent";
    const classificationLegend = classificationType === "Topic" ? "topic-view contribution" : "keyword-view contribution";
    return {
      nodes,
      links,
      displayedUsers: chosenUsers.length,
      displayedClassifications: classificationNodes.size,
      edgeLegend: `Blue width: ${engagementLegend} · Purple width: ${classificationLegend}`
    };
  }

  function renderNetwork(element, data, options) {
    const settings = Object.assign({
      height: 760,
      initialVideoCount: 24,
      maxSelected: 40,
      maxUsers: 100,
      labelCount: 18,
      engagementMode: "messages",
      classificationMode: "topics"
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

    let selected = new Set(startingIds.map(String));

    function update(nextSelected) {
      selected = nextSelected;
      const network = buildNetwork(data, selected, settings);
      status.textContent = `${selected.size} video${selected.size === 1 ? "" : "s"} selected · ${network.displayedUsers.toLocaleString()} chatters · ${network.displayedClassifications.toLocaleString()} classifications`;
      global.SunDataNetwork.renderViewerActivityNetwork(chart, network, {
        height: settings.height,
        showVideoPicker: false
      });
    }

    const picker = createCatalogPicker(data.videos, startingIds, {
      maxSelected: settings.maxSelected,
      summaryPrefix: "Videos",
      filterData: data.filters
    }, update);
    const engagementControl = createSelectControl("Chatter edges", [
      { value: "attendance", label: "Streams attended" },
      { value: "messages", label: "Messages sent" }
    ], settings.engagementMode, (value) => {
      settings.engagementMode = value;
      update(selected);
    });
    const classificationControl = createSelectControl("Stream classification", [
      { value: "topics", label: "Primary topic" },
      { value: "keywords", label: "Normalized keyword/tag" }
    ], settings.classificationMode, (value) => {
      settings.classificationMode = value;
      update(selected);
    });
    controls.append(picker, engagementControl, classificationControl, status);
    update(selected);
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
      summaryPrefix: "Compare",
      filterData: data.filters
    }, update);
    controls.append(picker, status);
    update(new Set(startingIds.map(String)));
  }

  global.SunDataVideoExplorer = Object.assign(global.SunDataVideoExplorer || {}, {
    renderNetwork,
    renderOverlap
  });
  global.SunDataVideoExplorerUtils = {
    ensureIndexes,
    initialIds,
    createCatalogPicker,
    createSelectControl,
    formatCount,
    formatPercent
  };
}(window));
