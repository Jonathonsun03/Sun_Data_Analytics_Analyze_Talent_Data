(function (global) {
  "use strict";

  const utils = global.SunDataVideoExplorerUtils;

  function emptyMessage(element, text) {
    element.replaceChildren();
    const message = document.createElement("p");
    message.className = "sd-video-explorer__empty";
    message.textContent = text;
    element.append(message);
  }

  function selectedActivity(data, selectedIds, settings) {
    const edgesByVideo = utils.ensureIndexes(data);
    const videoIndexById = new Map(data.videos.map((video, index) => [String(video.id), index]));
    const videos = Array.from(selectedIds)
      .map((id) => videoIndexById.get(String(id)))
      .filter((index) => index !== undefined)
      .map((index) => Object.assign({ index }, data.videos[index]));
    const activity = new Map();

    videos.forEach((video) => {
      edgesByVideo[video.index].forEach(([userIndex, messages]) => {
        const current = activity.get(userIndex) || { messages: 0, attendance: 0 };
        current.messages += messages;
        current.attendance += 1;
        activity.set(userIndex, current);
      });
    });

    const field = settings.engagementMode === "attendance" ? "attendance" : "messages";
    const rankedUsers = Array.from(activity.entries())
      .sort((a, b) => b[1][field] - a[1][field] || b[1].messages - a[1].messages || a[0] - b[0]);
    const chosenUsers = settings.selectedUserIndexes instanceof Set
      ? Array.from(settings.selectedUserIndexes)
        .map((index) => [index, activity.get(index)])
        .filter((entry) => entry[1])
      : rankedUsers.slice(0, Number(settings.maxUsers) || 50);
    const users = chosenUsers
      .map(([index, totals]) => ({
        index,
        label: data.users[index].label,
        messages: totals.messages,
        attendance: totals.attendance,
        value: totals[field]
      }));
    const userIndexes = new Set(users.map((user) => user.index));

    return { edgesByVideo, videos, users, userIndexes, field, activity, rankedUsers };
  }

  function createChatterPicker(data, settings, onChange) {
    const picker = document.createElement("details");
    picker.className = "sd-chatter-picker";
    const summary = document.createElement("summary");
    summary.className = "sd-video-picker__summary";
    picker.append(summary);
    const panel = document.createElement("div");
    panel.className = "sd-chatter-picker__panel";
    const search = document.createElement("input");
    search.type = "search";
    search.className = "sd-video-picker__search";
    search.placeholder = `Search all ${data.users.length.toLocaleString()} chatters`;
    search.setAttribute("aria-label", search.placeholder);
    const filters = document.createElement("div");
    filters.className = "sd-chatter-picker__filters";
    const scopeLabel = document.createElement("label");
    scopeLabel.textContent = "Count within";
    const scopeSelect = document.createElement("select");
    scopeSelect.setAttribute("aria-label", "Chatter activity count scope");
    [["selected", "Selected streams"], ["all", "All observed streams"]].forEach(([value, label]) => {
      const option = document.createElement("option");
      option.value = value;
      option.textContent = label;
      scopeSelect.append(option);
    });
    scopeLabel.append(scopeSelect);
    const measureLabel = document.createElement("label");
    measureLabel.textContent = "Measure";
    const measureSelect = document.createElement("select");
    measureSelect.setAttribute("aria-label", "Chatter activity measure");
    [["messages", "Messages sent"], ["attendance", "Streams attended"]].forEach(([value, label]) => {
      const option = document.createElement("option");
      option.value = value;
      option.textContent = label;
      measureSelect.append(option);
    });
    measureLabel.append(measureSelect);
    const sortLabel = document.createElement("label");
    sortLabel.textContent = "Order";
    const sortSelect = document.createElement("select");
    sortSelect.setAttribute("aria-label", "Order filtered chatters");
    [
      ["activity-desc", "Highest activity first"],
      ["activity-asc", "Lowest activity first"],
      ["name-asc", "Name A–Z"],
      ["name-desc", "Name Z–A"]
    ].forEach(([value, label]) => {
      const option = document.createElement("option");
      option.value = value;
      option.textContent = label;
      sortSelect.append(option);
    });
    sortLabel.append(sortSelect);
    const minimumLabel = document.createElement("label");
    minimumLabel.textContent = "At least";
    const minimumInput = document.createElement("input");
    minimumInput.type = "number";
    minimumInput.min = "0";
    minimumInput.step = "1";
    minimumInput.placeholder = "Any";
    minimumInput.setAttribute("aria-label", "Minimum chatter activity");
    minimumLabel.append(minimumInput);
    const maximumLabel = document.createElement("label");
    maximumLabel.textContent = "At most";
    const maximumInput = document.createElement("input");
    maximumInput.type = "number";
    maximumInput.min = "0";
    maximumInput.step = "1";
    maximumInput.placeholder = "Any";
    maximumInput.setAttribute("aria-label", "Maximum chatter activity");
    maximumLabel.append(maximumInput);
    const clearFiltersButton = document.createElement("button");
    clearFiltersButton.type = "button";
    clearFiltersButton.textContent = "Clear filters";
    filters.append(scopeLabel, measureLabel, sortLabel, minimumLabel, maximumLabel, clearFiltersButton);
    const status = document.createElement("div");
    status.className = "sd-video-picker__match-status";
    const actions = document.createElement("div");
    actions.className = "sd-video-picker__actions";
    const automaticButton = document.createElement("button");
    automaticButton.type = "button";
    automaticButton.textContent = "Use automatic";
    const noneButton = document.createElement("button");
    noneButton.type = "button";
    noneButton.textContent = "None";
    const selectMatchingButton = document.createElement("button");
    selectMatchingButton.type = "button";
    selectMatchingButton.textContent = "Select matching";
    const deselectMatchingButton = document.createElement("button");
    deselectMatchingButton.type = "button";
    deselectMatchingButton.textContent = "Deselect matching";
    const selectPageButton = document.createElement("button");
    selectPageButton.type = "button";
    selectPageButton.textContent = "Select page";
    const deselectPageButton = document.createElement("button");
    deselectPageButton.type = "button";
    deselectPageButton.textContent = "Deselect page";
    actions.append(
      automaticButton,
      noneButton,
      selectMatchingButton,
      deselectMatchingButton,
      selectPageButton,
      deselectPageButton
    );
    const notice = document.createElement("div");
    notice.className = "sd-video-picker__notice";
    const list = document.createElement("div");
    list.className = "sd-video-picker__options sd-chatter-picker__options";
    const pagination = document.createElement("div");
    pagination.className = "sd-chatter-picker__pagination";
    const previousButton = document.createElement("button");
    previousButton.type = "button";
    previousButton.textContent = "Previous";
    const pageLabel = document.createElement("label");
    pageLabel.textContent = "Page";
    const pageInput = document.createElement("input");
    pageInput.type = "number";
    pageInput.min = "1";
    pageInput.step = "1";
    pageInput.setAttribute("aria-label", "Chatter results page");
    const pageStatus = document.createElement("span");
    pageLabel.append(pageInput, pageStatus);
    const nextButton = document.createElement("button");
    nextButton.type = "button";
    nextButton.textContent = "Next";
    pagination.append(previousButton, pageLabel, nextButton);
    panel.append(search, filters, status, actions, notice, list, pagination);
    picker.append(panel);

    let automaticIndexes = [];
    let activity = new Map();
    let manualSelection = null;
    let matchingIndexes = [];
    let page = 0;
    const maxUsers = Number(settings.maxUsers) || 50;
    const pageSize = 100;
    const globalActivity = new Map();
    (data.edges || []).forEach((edge) => {
      const userIndex = Number(edge[1]);
      const messages = Number(edge[2]) || 0;
      const current = globalActivity.get(userIndex) || { messages: 0, attendance: 0 };
      current.messages += messages;
      current.attendance += 1;
      globalActivity.set(userIndex, current);
    });

    function activeSelection() {
      return manualSelection === null ? new Set(automaticIndexes) : new Set(manualSelection);
    }

    function resultIndexes() {
      const query = search.value.trim().toLocaleLowerCase();
      const sourceActivity = scopeSelect.value === "all" ? globalActivity : activity;
      const field = measureSelect.value;
      const order = sortSelect.value;
      const minimum = minimumInput.value === "" ? null : Number(minimumInput.value);
      const maximum = maximumInput.value === "" ? null : Number(maximumInput.value);
      return data.users
        .map((user, index) => ({
          index,
          label: String(user.label).toLocaleLowerCase(),
          totals: sourceActivity.get(index) || { messages: 0, attendance: 0 }
        }))
        .filter((item) => {
          const value = item.totals[field];
          return (!query || item.label.includes(query)) &&
            (minimum === null || value >= minimum) &&
            (maximum === null || value <= maximum);
        })
        .sort((left, right) => {
          if (order === "name-asc") return left.label.localeCompare(right.label);
          if (order === "name-desc") return right.label.localeCompare(left.label);
          const direction = order === "activity-asc" ? 1 : -1;
          return direction * (left.totals[field] - right.totals[field]) ||
            direction * (left.totals.messages - right.totals.messages) ||
            direction * (left.totals.attendance - right.totals.attendance) ||
            left.label.localeCompare(right.label);
        })
        .map((item) => item.index);
    }

    function currentPageIndexes() {
      return matchingIndexes.slice(page * pageSize, (page + 1) * pageSize);
    }

    function updateSummary() {
      const current = activeSelection();
      summary.textContent = manualSelection === null
        ? `Chatters: automatic top ${automaticIndexes.length}`
        : `Chatters: ${current.size} selected`;
      const pageStart = matchingIndexes.length === 0 ? 0 : page * pageSize + 1;
      const pageEnd = Math.min((page + 1) * pageSize, matchingIndexes.length);
      const scopeText = scopeSelect.value === "all" ? "all observed streams" : "selected streams";
      status.textContent = `${matchingIndexes.length.toLocaleString()} matching across ${scopeText} · showing ${pageStart.toLocaleString()}–${pageEnd.toLocaleString()}`;
      notice.textContent = manualSelection === null
        ? `The chart still uses its automatic starting set. Change a checkbox or select matching results to customize up to ${maxUsers} chatters.`
        : `Choose up to ${maxUsers} chatters. Chatters without activity in the selected streams remain selected but are not drawn.`;
      const pageCount = Math.max(1, Math.ceil(matchingIndexes.length / pageSize));
      pageInput.max = String(pageCount);
      pageInput.value = String(Math.min(page + 1, pageCount));
      pageStatus.textContent = `of ${pageCount}`;
      previousButton.disabled = page === 0;
      nextButton.disabled = page + 1 >= pageCount;
    }

    function renderRows(resetPage) {
      if (resetPage) page = 0;
      matchingIndexes = resultIndexes();
      const pageCount = Math.max(1, Math.ceil(matchingIndexes.length / pageSize));
      page = Math.min(page, pageCount - 1);
      const current = activeSelection();
      list.replaceChildren();
      const pageIndexes = currentPageIndexes();
      pageIndexes.forEach((index) => {
        const user = data.users[index];
        const selectedTotals = activity.get(index) || { messages: 0, attendance: 0 };
        const allTotals = globalActivity.get(index) || { messages: 0, attendance: 0 };
        const row = document.createElement("label");
        row.className = "sd-video-picker__option sd-chatter-picker__option";
        const checkbox = document.createElement("input");
        checkbox.type = "checkbox";
        checkbox.checked = current.has(index);
        const text = document.createElement("span");
        const details = scopeSelect.value === "all"
          ? `${utils.formatCount(allTotals.attendance)} streams · ${utils.formatCount(allTotals.messages)} messages overall`
          : `${utils.formatCount(selectedTotals.attendance)} selected streams · ${utils.formatCount(selectedTotals.messages)} messages`;
        text.textContent = `${user.label} · ${details}`;
        checkbox.addEventListener("change", () => {
          if (manualSelection === null) manualSelection = new Set(automaticIndexes);
          if (checkbox.checked && manualSelection.size >= maxUsers) {
            checkbox.checked = false;
            notice.textContent = `Selection limit reached (${maxUsers}). Clear another chatter first.`;
            return;
          }
          if (checkbox.checked) manualSelection.add(index);
          else manualSelection.delete(index);
          updateSummary();
          onChange(manualSelection);
        });
        row.append(checkbox, text);
        list.append(row);
      });
      updateSummary();
    }

    function setManual(nextSelection) {
      manualSelection = nextSelection;
      renderRows();
      onChange(manualSelection);
    }

    search.addEventListener("input", () => renderRows(true));
    scopeSelect.addEventListener("change", () => renderRows(true));
    measureSelect.addEventListener("change", () => renderRows(true));
    sortSelect.addEventListener("change", () => renderRows(true));
    minimumInput.addEventListener("input", () => renderRows(true));
    maximumInput.addEventListener("input", () => renderRows(true));
    clearFiltersButton.addEventListener("click", () => {
      search.value = "";
      scopeSelect.value = "selected";
      measureSelect.value = "messages";
      sortSelect.value = "activity-desc";
      minimumInput.value = "";
      maximumInput.value = "";
      renderRows(true);
    });
    previousButton.addEventListener("click", () => {
      if (page > 0) page -= 1;
      renderRows(false);
    });
    nextButton.addEventListener("click", () => {
      if ((page + 1) * pageSize < matchingIndexes.length) page += 1;
      renderRows(false);
    });
    function goToEnteredPage() {
      const pageCount = Math.max(1, Math.ceil(matchingIndexes.length / pageSize));
      const requestedPage = Number(pageInput.value);
      const safePage = Number.isFinite(requestedPage)
        ? Math.max(1, Math.min(pageCount, Math.trunc(requestedPage)))
        : page + 1;
      page = safePage - 1;
      renderRows(false);
    }
    pageInput.addEventListener("change", goToEnteredPage);
    pageInput.addEventListener("keydown", (event) => {
      if (event.key !== "Enter") return;
      event.preventDefault();
      goToEnteredPage();
    });
    automaticButton.addEventListener("click", () => setManual(null));
    noneButton.addEventListener("click", () => setManual(new Set()));
    selectMatchingButton.addEventListener("click", () => {
      if (manualSelection === null) manualSelection = new Set();
      let added = 0;
      for (const index of matchingIndexes) {
        if (manualSelection.size >= maxUsers) break;
        if (!manualSelection.has(index)) {
          manualSelection.add(index);
          added += 1;
        }
      }
      renderRows();
      const skipped = matchingIndexes.filter((index) => !manualSelection.has(index)).length;
      notice.textContent = `${added} matching chatter${added === 1 ? " was" : "s were"} added.` +
        (skipped > 0 ? ` ${skipped} could not be added because the limit is ${maxUsers}.` : "");
      onChange(manualSelection);
    });
    deselectMatchingButton.addEventListener("click", () => {
      if (manualSelection === null) manualSelection = new Set(automaticIndexes);
      matchingIndexes.forEach((index) => manualSelection.delete(index));
      renderRows();
      notice.textContent = "Matching chatters were removed from the selection.";
      onChange(manualSelection);
    });
    selectPageButton.addEventListener("click", () => {
      if (manualSelection === null) manualSelection = new Set();
      const pageIndexes = currentPageIndexes();
      let added = 0;
      for (const index of pageIndexes) {
        if (manualSelection.size >= maxUsers) break;
        if (!manualSelection.has(index)) {
          manualSelection.add(index);
          added += 1;
        }
      }
      renderRows();
      const skipped = pageIndexes.filter((index) => !manualSelection.has(index)).length;
      notice.textContent = `${added} chatter${added === 1 ? " was" : "s were"} added from this page.` +
        (skipped > 0 ? ` ${skipped} could not be added because the limit is ${maxUsers}.` : "");
      onChange(manualSelection);
    });
    deselectPageButton.addEventListener("click", () => {
      if (manualSelection === null) manualSelection = new Set(automaticIndexes);
      currentPageIndexes().forEach((index) => manualSelection.delete(index));
      renderRows();
      notice.textContent = "Chatters on this page were removed from the selection.";
      onChange(manualSelection);
    });

    return {
      element: picker,
      getSelection: () => manualSelection === null ? null : new Set(manualSelection),
      updateContext(nextAutomaticIndexes, nextActivity) {
        automaticIndexes = nextAutomaticIndexes.slice(0, maxUsers);
        activity = nextActivity;
        renderRows();
      }
    };
  }

  function makePickersExclusive(...pickers) {
    pickers.forEach((picker) => {
      picker.addEventListener("toggle", () => {
        if (!picker.open) return;
        pickers.forEach((other) => {
          if (other !== picker) other.open = false;
        });
      });
    });
  }

  function classificationRows(data, mode) {
    return data.classifications?.[mode] || [];
  }

  function renderSankeyChart(element, data, selectedIds, settings) {
    const selection = selectedActivity(data, selectedIds, settings);
    if (selection.videos.length === 0 || selection.users.length === 0) {
      emptyMessage(element, "Choose streams with collected chatter activity to build the alluvial view.");
      return { users: 0, classifications: 0 };
    }

    const userByIndex = new Map(selection.users.map((user) => [user.index, user]));
    const userNodes = selection.users.map((user) => ({
      id: `user:${user.index}`,
      label: user.label,
      value: 0,
      details: `Chatter: ${user.label} · Messages sent: ${utils.formatCount(user.messages)} · Streams attended: ${utils.formatCount(user.attendance)}`
    }));
    const videoNodes = selection.videos.map((video) => ({
      id: `video:${video.id}`,
      label: video.label,
      video,
      value: 0,
      details: `Video: ${video.label}`
    }));
    const userNodeById = new Map(userNodes.map((node) => [node.id, node]));
    const videoNodeById = new Map(videoNodes.map((node) => [node.id, node]));
    const userVideoLinks = [];

    selection.videos.forEach((video) => {
      selection.edgesByVideo[video.index].forEach(([userIndex, messages]) => {
        if (!selection.userIndexes.has(userIndex)) return;
        const value = selection.field === "attendance" ? 1 : messages;
        const source = userNodeById.get(`user:${userIndex}`);
        const target = videoNodeById.get(`video:${video.id}`);
        source.value += value;
        target.value += value;
        userVideoLinks.push({
          source,
          target,
          value,
          details: `Chatter: ${userByIndex.get(userIndex).label} · Video: ${video.label} · Messages sent: ${utils.formatCount(messages)} · Attendance: 1`
        });
      });
    });

    const classType = settings.classificationMode === "keywords" ? "Normalized keyword" : "Primary topic";
    const rowsByVideo = new Map();
    classificationRows(data, settings.classificationMode).forEach((row) => {
      const key = String(row.videoId);
      if (!rowsByVideo.has(key)) rowsByVideo.set(key, []);
      rowsByVideo.get(key).push(row);
    });
    const classNodesById = new Map();
    const videoClassLinks = [];

    videoNodes.filter((node) => node.value > 0).forEach((videoNode) => {
      const rows = rowsByVideo.get(String(videoNode.video.id)) || [];
      const destinations = rows.length > 0 ? rows : [{
        label: `No eligible ${classType.toLocaleLowerCase()}`,
        contributionPercentage: null,
        videoViews: null,
        classificationViews: null,
        confidence: null
      }];
      const value = videoNode.value / destinations.length;
      destinations.forEach((row) => {
        const nodeId = `classification:${row.label}`;
        if (!classNodesById.has(nodeId)) {
          classNodesById.set(nodeId, {
            id: nodeId,
            label: row.label,
            value: 0,
            details: `${classType}: ${row.label}`
          });
        }
        const target = classNodesById.get(nodeId);
        target.value += value;
        videoClassLinks.push({
          source: videoNode,
          target,
          value,
          details: `Video title: ${videoNode.video.label} · ${classType}: ${row.label} · Alluvial ${selection.field}: ${utils.formatCount(value)} · Video views: ${utils.formatCount(row.videoViews)} · Total classification views: ${utils.formatCount(row.classificationViews)} · View contribution: ${utils.formatPercent(row.contributionPercentage, 2)} · Classification confidence: ${utils.formatPercent(row.confidence === null || row.confidence === undefined ? null : Number(row.confidence) * 100, 0)}`
        });
      });
    });

    const activeUsers = userNodes.filter((node) => node.value > 0);
    const activeVideos = videoNodes.filter((node) => node.value > 0);
    const classNodes = Array.from(classNodesById.values()).sort((a, b) => b.value - a.value || a.label.localeCompare(b.label));
    const width = 1200;
    const height = Math.max(
      Number(settings.height) || 820,
      activeUsers.length * 18 + 100,
      activeVideos.length * 24 + 100,
      classNodes.length * 20 + 100
    );
    const top = 55;
    const bottom = 30;
    const available = height - top - bottom;
    const gap = 5;
    const total = activeUsers.reduce((sum, node) => sum + node.value, 0);
    const columns = [activeUsers, activeVideos, classNodes];
    const scale = Math.max(0.01, Math.min(...columns.map((nodes) => (
      (available - Math.max(0, nodes.length - 1) * gap) / Math.max(total, 1)
    ))));
    const xPositions = [175, 600, 1025];

    columns.forEach((nodes, columnIndex) => {
      const used = nodes.reduce((sum, node) => sum + node.value * scale, 0) + Math.max(0, nodes.length - 1) * gap;
      let y = top + Math.max(0, (available - used) / 2);
      nodes.forEach((node) => {
        node.x = xPositions[columnIndex];
        node.y = y;
        node.height = node.value * scale;
        node.sourceOffset = 0;
        node.targetOffset = 0;
        y += node.height + gap;
      });
    });

    function positionLinks(links) {
      links.forEach((link) => {
        link.sourceY = link.source.y + link.source.sourceOffset + link.value * scale / 2;
        link.targetY = link.target.y + link.target.targetOffset + link.value * scale / 2;
        link.source.sourceOffset += link.value * scale;
        link.target.targetOffset += link.value * scale;
      });
    }
    userVideoLinks.sort((a, b) => a.source.y - b.source.y || a.target.y - b.target.y);
    videoClassLinks.sort((a, b) => a.source.y - b.source.y || a.target.y - b.target.y);
    positionLinks(userVideoLinks);
    positionLinks(videoClassLinks);

    element.replaceChildren();
    const frame = document.createElement("div");
    frame.className = "sd-relationship-view__frame";
    element.append(frame);
    const svg = global.d3.select(frame)
      .append("svg")
      .attr("viewBox", [0, 0, width, height])
      .attr("role", "img")
      .attr("aria-label", "Alluvial chart of chatters flowing through streams to stream-title classifications");

    const headings = ["Chatters", "Streams", classType === "Primary topic" ? "Primary topics" : "Normalized keywords"];
    svg.selectAll("text.sd-relationship-view__heading")
      .data(headings)
      .join("text")
      .attr("class", "sd-relationship-view__heading")
      .attr("x", (_, index) => xPositions[index])
      .attr("y", 28)
      .attr("text-anchor", "middle")
      .text((label) => label);

    function drawLinks(links, color, className) {
      return svg.append("g")
        .selectAll("path")
        .data(links)
        .join("path")
        .attr("class", `sd-alluvial__link ${className}`)
        .attr("d", (link) => {
          const sourceX = link.source.x + 7;
          const targetX = link.target.x - 7;
          const middleX = (sourceX + targetX) / 2;
          return `M${sourceX},${link.sourceY} C${middleX},${link.sourceY} ${middleX},${link.targetY} ${targetX},${link.targetY}`;
        })
        .attr("stroke", color)
        .attr("stroke-width", (link) => Math.max(0.8, link.value * scale))
        .each(function (link) {
          global.d3.select(this).append("title").text(link.details);
        });
    }
    const userVideoPaths = drawLinks(userVideoLinks, "#60a5fa", "sd-alluvial__link--engagement");
    const videoClassPaths = drawLinks(videoClassLinks, "#a78bfa", "sd-alluvial__link--classification");

    const nodeGroups = svg.append("g")
      .selectAll("g")
      .data(columns.flat())
      .join("g")
      .attr("class", "sd-alluvial__node")
      .attr("tabindex", (node) => node.id.startsWith("user:") ? 0 : null)
      .attr("role", (node) => node.id.startsWith("user:") ? "button" : null)
      .attr("aria-label", (node) => node.id.startsWith("user:") ? `Highlight every path for chatter ${node.label}` : null);
    nodeGroups.append("rect")
      .attr("x", (node) => node.x - 7)
      .attr("y", (node) => node.y)
      .attr("width", 14)
      .attr("height", (node) => Math.max(1, node.height))
      .attr("rx", 2)
      .attr("fill", (node) => node.id.startsWith("user:") ? "#3b82f6" : node.id.startsWith("video:") ? "#f59e0b" : "#8b5cf6");
    nodeGroups.append("title").text((node) => `${node.details} · Flow: ${utils.formatCount(node.value)}`);
    nodeGroups.append("text")
      .attr("class", "sd-alluvial__label")
      .attr("x", (node) => node.x + (node.id.startsWith("user:") ? -11 : 11))
      .attr("y", (node) => node.y + node.height / 2 + 3)
      .attr("text-anchor", (node) => node.id.startsWith("user:") ? "end" : "start")
      .text((node) => node.label.length > 30 ? `${node.label.slice(0, 29)}…` : node.label);

    function resetChatterHighlight() {
      userVideoPaths
        .classed("sd-alluvial__link--dimmed", false)
        .classed("sd-alluvial__link--highlighted-engagement", false);
      videoClassPaths
        .classed("sd-alluvial__link--dimmed", false)
        .classed("sd-alluvial__link--highlighted-classification", false);
      nodeGroups
        .classed("sd-alluvial__node--dimmed", false)
        .classed("sd-alluvial__node--highlighted", false)
        .classed("sd-alluvial__node--selected", false);
    }

    function highlightChatter(chatterNode) {
      const directLinks = new Set(userVideoLinks.filter((link) => link.source === chatterNode));
      const videoIds = new Set(Array.from(directLinks, (link) => link.target.id));
      const downstreamLinks = new Set(videoClassLinks.filter((link) => videoIds.has(link.source.id)));
      const relatedNodeIds = new Set([chatterNode.id]);
      directLinks.forEach((link) => relatedNodeIds.add(link.target.id));
      downstreamLinks.forEach((link) => relatedNodeIds.add(link.target.id));

      userVideoPaths
        .classed("sd-alluvial__link--dimmed", (link) => !directLinks.has(link))
        .classed("sd-alluvial__link--highlighted-engagement", (link) => directLinks.has(link));
      videoClassPaths
        .classed("sd-alluvial__link--dimmed", (link) => !downstreamLinks.has(link))
        .classed("sd-alluvial__link--highlighted-classification", (link) => downstreamLinks.has(link));
      nodeGroups
        .classed("sd-alluvial__node--dimmed", (node) => !relatedNodeIds.has(node.id))
        .classed("sd-alluvial__node--highlighted", (node) => relatedNodeIds.has(node.id))
        .classed("sd-alluvial__node--selected", (node) => node === chatterNode);
    }

    nodeGroups.filter((node) => node.id.startsWith("user:"))
      .on("mouseenter", function (event, node) {
        highlightChatter(node);
      })
      .on("mouseleave", resetChatterHighlight)
      .on("focus", function (event, node) {
        highlightChatter(node);
      })
      .on("blur", resetChatterHighlight)
      .on("keydown", function (event) {
        if (event.key === "Escape") {
          this.blur();
          resetChatterHighlight();
        }
      });

    return { users: activeUsers.length, classifications: classNodes.length };
  }

  function renderSankey(element, data, options) {
    if (!utils || !global.d3) throw new Error("Video explorer utilities and D3 are required.");
    const settings = Object.assign({
      height: 820,
      initialVideoCount: 12,
      maxSelected: 20,
      maxUsers: 30,
      engagementMode: "attendance",
      classificationMode: "topics"
    }, options || {});
    const startingIds = utils.initialIds(data, settings);
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
      const automatic = selectedActivity(
        data,
        selected,
        Object.assign({}, settings, { selectedUserIndexes: null })
      );
      chatterPicker.updateContext(automatic.users.map((user) => user.index), automatic.activity);
      settings.selectedUserIndexes = chatterPicker.getSelection();
      const result = renderSankeyChart(chart, data, selected, settings);
      status.textContent = `${selected.size} streams · ${result.users} chatters · ${result.classifications} classifications`;
    }

    const picker = utils.createCatalogPicker(data.videos, startingIds, {
      maxSelected: settings.maxSelected,
      summaryPrefix: "Streams",
      filterData: data.filters
    }, update);
    const chatterPicker = createChatterPicker(data, settings, (selection) => {
      settings.selectedUserIndexes = selection;
      update(selected);
    });
    makePickersExclusive(picker, chatterPicker.element);
    const engagement = utils.createSelectControl("Flow width", [
      { value: "attendance", label: "Streams attended" },
      { value: "messages", label: "Messages sent" }
    ], settings.engagementMode, (value) => {
      settings.engagementMode = value;
      update(selected);
    });
    const classification = utils.createSelectControl("Stream classification", [
      { value: "topics", label: "Primary topic" },
      { value: "keywords", label: "Normalized keyword/tag" }
    ], settings.classificationMode, (value) => {
      settings.classificationMode = value;
      update(selected);
    });
    controls.append(picker, chatterPicker.element, engagement, classification, status);
    update(selected);
  }

  function similarityOrder(rows) {
    if (rows.length < 3) return rows;
    const remaining = rows.slice(1);
    const ordered = [rows[0]];
    function cosine(left, right) {
      let dot = 0;
      let leftSquared = 0;
      let rightSquared = 0;
      left.values.forEach((value, index) => {
        dot += value * right.values[index];
        leftSquared += value * value;
        rightSquared += right.values[index] * right.values[index];
      });
      return leftSquared > 0 && rightSquared > 0 ? dot / Math.sqrt(leftSquared * rightSquared) : 0;
    }
    while (remaining.length > 0) {
      const previous = ordered[ordered.length - 1];
      let bestIndex = 0;
      let bestSimilarity = -1;
      remaining.forEach((candidate, index) => {
        const similarity = cosine(previous, candidate);
        if (similarity > bestSimilarity || (similarity === bestSimilarity && candidate.total > remaining[bestIndex].total)) {
          bestSimilarity = similarity;
          bestIndex = index;
        }
      });
      ordered.push(remaining.splice(bestIndex, 1)[0]);
    }
    return ordered;
  }

  function renderHeatmapChart(element, data, selectedIds, settings) {
    const selection = selectedActivity(data, selectedIds, settings);
    if (selection.videos.length === 0 || selection.users.length === 0) {
      emptyMessage(element, "Choose streams with collected chatter activity to build the heatmap.");
      return { users: 0 };
    }
    const videoPosition = new Map(selection.videos.map((video, index) => [video.index, index]));
    let rows = selection.users.map((user) => {
      const values = Array(selection.videos.length).fill(0);
      selection.videos.forEach((video) => {
        selection.edgesByVideo[video.index].forEach(([userIndex, messages]) => {
          if (userIndex === user.index) values[videoPosition.get(video.index)] = selection.field === "attendance" ? 1 : messages;
        });
      });
      return Object.assign({}, user, { values, total: values.reduce((sum, value) => sum + value, 0) });
    });
    if (settings.rowOrder === "similarity") rows = similarityOrder(rows);

    const compact = Boolean(settings.compact);
    const cellWidth = Math.max(
      compact ? 16 : 18,
      Math.min(compact ? 28 : 32, (compact ? 620 : 720) / selection.videos.length)
    );
    const cellHeight = compact ? 14 : 17;
    const margin = compact
      ? { top: 128, right: 24, bottom: 36, left: 165 }
      : { top: 190, right: 35, bottom: 45, left: 205 };
    const width = margin.left + selection.videos.length * cellWidth + margin.right;
    const height = margin.top + rows.length * cellHeight + margin.bottom;
    const maximum = global.d3.max(rows.flatMap((row) => row.values)) || 1;
    const color = selection.field === "attendance"
      ? global.d3.scaleLinear().domain([0, 1]).range(["#f8fafc", "#2563eb"])
      : global.d3.scaleSequential(global.d3.interpolateBlues).domain([0, maximum]);

    element.replaceChildren();
    const frame = document.createElement("div");
    frame.className = "sd-relationship-view__frame sd-heatmap__frame";
    element.append(frame);
    const svg = global.d3.select(frame)
      .append("svg")
      .attr("viewBox", [0, 0, width, height])
      .attr("role", "img")
      .attr("aria-label", "Heatmap of chatter participation across selected streams");

    svg.append("text")
      .attr("class", "sd-relationship-view__heading")
      .attr("x", margin.left - 12)
      .attr("y", margin.top - 18)
      .attr("text-anchor", "end")
      .text("Chatters");
    svg.append("text")
      .attr("class", "sd-relationship-view__heading")
      .attr("x", margin.left + selection.videos.length * cellWidth / 2)
      .attr("y", 24)
      .attr("text-anchor", "middle")
      .text("Streams");

    svg.append("g")
      .selectAll("text")
      .data(selection.videos)
      .join("text")
      .attr("class", "sd-heatmap__column-label")
      .attr("transform", (_, index) => `translate(${margin.left + index * cellWidth + cellWidth / 2},${margin.top - 10}) rotate(-55)`)
      .attr("text-anchor", "start")
      .text((video) => {
        const limit = compact ? 23 : 34;
        return video.label.length > limit ? `${video.label.slice(0, limit - 1)}…` : video.label;
      })
      .append("title")
      .text((video) => video.label);

    svg.append("g")
      .selectAll("text")
      .data(rows)
      .join("text")
      .attr("class", "sd-heatmap__row-label")
      .attr("x", margin.left - 8)
      .attr("y", (_, index) => margin.top + index * cellHeight + cellHeight * 0.72)
      .attr("text-anchor", "end")
      .text((row) => {
        const limit = compact ? 21 : 28;
        return row.label.length > limit ? `${row.label.slice(0, limit - 1)}…` : row.label;
      })
      .append("title")
      .text((row) => row.label);

    const cells = rows.flatMap((row, rowIndex) => row.values.map((value, columnIndex) => ({
      row,
      rowIndex,
      video: selection.videos[columnIndex],
      columnIndex,
      value
    })));
    svg.append("g")
      .selectAll("rect")
      .data(cells)
      .join("rect")
      .attr("class", "sd-heatmap__cell")
      .attr("x", (cell) => margin.left + cell.columnIndex * cellWidth)
      .attr("y", (cell) => margin.top + cell.rowIndex * cellHeight)
      .attr("width", cellWidth - 1)
      .attr("height", cellHeight - 1)
      .attr("fill", (cell) => color(cell.value))
      .each(function (cell) {
        const messages = selection.edgesByVideo[cell.video.index]
          .find(([userIndex]) => userIndex === cell.row.index)?.[1] || 0;
        global.d3.select(this).append("title").text(
          `Chatter: ${cell.row.label} · Video: ${cell.video.label} · Messages sent: ${utils.formatCount(messages)} · Attendance: ${messages > 0 ? 1 : 0}`
        );
      });

    const legend = svg.append("g").attr("transform", `translate(${margin.left},${height - 20})`);
    legend.append("text")
      .attr("class", "sd-heatmap__legend-label")
      .text(selection.field === "attendance" ? "Blue = attended" : `Darker blue = more messages (maximum ${utils.formatCount(maximum)})`);
    return { users: rows.length };
  }

  function renderHeatmap(element, data, options) {
    if (!utils || !global.d3) throw new Error("Video explorer utilities and D3 are required.");
    const settings = Object.assign({
      initialVideoCount: 24,
      maxSelected: 40,
      maxUsers: 80,
      engagementMode: "attendance",
      rowOrder: "similarity"
    }, options || {});
    const startingIds = utils.initialIds(data, settings);
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
      const automatic = selectedActivity(
        data,
        selected,
        Object.assign({}, settings, { selectedUserIndexes: null })
      );
      chatterPicker.updateContext(automatic.users.map((user) => user.index), automatic.activity);
      settings.selectedUserIndexes = chatterPicker.getSelection();
      const result = renderHeatmapChart(chart, data, selected, settings);
      status.textContent = `${selected.size} streams · ${result.users} chatters displayed`;
    }
    const picker = utils.createCatalogPicker(data.videos, startingIds, {
      maxSelected: settings.maxSelected,
      summaryPrefix: "Streams",
      filterData: data.filters
    }, update);
    const chatterPicker = createChatterPicker(data, settings, (selection) => {
      settings.selectedUserIndexes = selection;
      update(selected);
    });
    makePickersExclusive(picker, chatterPicker.element);
    const engagement = utils.createSelectControl("Cell measure", [
      { value: "attendance", label: "Attendance" },
      { value: "messages", label: "Messages sent" }
    ], settings.engagementMode, (value) => {
      settings.engagementMode = value;
      update(selected);
    });
    const order = utils.createSelectControl("Row order", [
      { value: "similarity", label: "Similar participation" },
      { value: "activity", label: "Highest activity" }
    ], settings.rowOrder, (value) => {
      settings.rowOrder = value;
      update(selected);
    });
    controls.append(picker, chatterPicker.element, engagement, order, status);
    update(selected);
  }

  global.SunDataVideoExplorer = Object.assign(global.SunDataVideoExplorer || {}, {
    renderSankey,
    renderHeatmap
  });
}(window));
