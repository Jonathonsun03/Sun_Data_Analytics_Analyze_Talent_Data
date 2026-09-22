(function (global) {
  "use strict";

  const TYPE_COLORS = {
    User: "#3b82f6",
    Video: "#f59e0b",
    Streamer: "#8b5cf6",
    Topic: "#8b5cf6",
    Keyword: "#14b8a6"
  };

  const TYPE_LABELS = {
    User: "Chatter",
    Video: "Video",
    Streamer: "Streamer",
    Topic: "Primary topic",
    Keyword: "Normalized keyword"
  };

  const TYPE_HEADINGS = {
    User: "Chatters",
    Video: "Videos",
    Streamer: "Streamers",
    Topic: "Primary topics",
    Keyword: "Normalized keywords"
  };

  const TYPE_X = {
    User: 120,
    Video: 520,
    Streamer: 900,
    Topic: 900,
    Keyword: 900
  };

  function createVideoPicker(videos, onChange) {
    const picker = document.createElement("details");
    picker.className = "sd-video-picker";
    const summary = document.createElement("summary");
    summary.className = "sd-video-picker__summary";
    picker.append(summary);

    const panel = document.createElement("div");
    panel.className = "sd-video-picker__panel";
    const search = document.createElement("input");
    search.className = "sd-video-picker__search";
    search.type = "search";
    search.placeholder = "Filter video titles";
    search.setAttribute("aria-label", search.placeholder);
    const actions = document.createElement("div");
    actions.className = "sd-video-picker__actions";
    const allButton = document.createElement("button");
    allButton.type = "button";
    allButton.textContent = "All";
    const noneButton = document.createElement("button");
    noneButton.type = "button";
    noneButton.textContent = "None";
    actions.append(allButton, noneButton);
    const options = document.createElement("div");
    options.className = "sd-video-picker__options";
    panel.append(search, actions, options);
    picker.append(panel);

    const selected = new Set(videos.map((video) => video.id));
    const rows = videos.map((video) => {
      const row = document.createElement("label");
      row.className = "sd-video-picker__option";
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.checked = true;
      checkbox.value = video.id;
      const label = document.createElement("span");
      label.textContent = video.label;
      row.append(checkbox, label);
      options.append(row);
      checkbox.addEventListener("change", function () {
        if (checkbox.checked) selected.add(video.id);
        else selected.delete(video.id);
        updateSummary();
        onChange(new Set(selected));
      });
      return { row, checkbox, label: String(video.label).toLocaleLowerCase() };
    });

    function updateSummary() {
      summary.textContent = selected.size === videos.length
        ? `Videos: all ${videos.length}`
        : `Videos: ${selected.size} of ${videos.length}`;
    }

    function setAll(checked) {
      selected.clear();
      rows.forEach((row) => {
        row.checkbox.checked = checked;
        if (checked) selected.add(row.checkbox.value);
      });
      updateSummary();
      onChange(new Set(selected));
    }

    search.addEventListener("input", function () {
      const query = search.value.trim().toLocaleLowerCase();
      rows.forEach((row) => {
        row.row.hidden = Boolean(query) && !row.label.includes(query);
      });
    });
    allButton.addEventListener("click", () => setAll(true));
    noneButton.addEventListener("click", () => setAll(false));
    updateSummary();
    return picker;
  }

  /**
   * Render an interactive, layered network.
   *
   * data.nodes: [{id, type, label, weight, showLabel}]
   * data.links: [{source, target, type, weight}]
   *
   * Node ids must be unique. Link source and target values refer to node ids.
   */
  function renderViewerActivityNetwork(element, data, options) {
    if (!element) throw new Error("A network container element is required.");
    if (!global.d3) throw new Error("D3 must be loaded before the network renderer.");

    const settings = Object.assign({ height: 680 }, options || {});
    const width = 1020;
    const height = Number(settings.height) || 680;
    const nodes = (data.nodes || []).map((node) => Object.assign({}, node));
    const links = (data.links || []).map((link) => Object.assign({}, link));

    if (element.__sdSimulation) element.__sdSimulation.stop();
    element.replaceChildren();
    element.classList.add("sd-network");

    const toolbar = document.createElement("div");
    toolbar.className = "sd-network__toolbar";
    const search = document.createElement("input");
    search.className = "sd-network__search";
    search.type = "search";
    search.placeholder = "Find a chatter, video, topic, or keyword";
    search.setAttribute("aria-label", search.placeholder);
    const reset = document.createElement("button");
    reset.className = "sd-network__reset";
    reset.type = "button";
    reset.textContent = "Reset view";
    const legend = document.createElement("div");
    legend.className = "sd-network__legend";
    legend.setAttribute("aria-label", "Network legend");
    const presentTypes = new Set(nodes.map((item) => item.type));
    Object.entries(TYPE_COLORS).filter(([type]) => presentTypes.has(type)).forEach(([type, color]) => {
      const item = document.createElement("span");
      item.className = "sd-network__legend-item";
      const swatch = document.createElement("span");
      swatch.className = "sd-network__legend-swatch";
      swatch.style.backgroundColor = color;
      item.append(swatch, document.createTextNode(TYPE_LABELS[type]));
      legend.append(item);
    });
    const edgeLegend = document.createElement("span");
    edgeLegend.className = "sd-network__legend-item";
    edgeLegend.textContent = data.edgeLegend || "Line width shows relationship weight";
    legend.append(edgeLegend);
    const videoNodes = nodes.filter((item) => item.type === "Video");
    let selectedVideoIds = new Set(videoNodes.map((item) => item.id));
    toolbar.append(search);
    if (settings.showVideoPicker !== false) {
      const videoPicker = createVideoPicker(videoNodes, function (selected) {
        selectedVideoIds = selected;
        applyVideoSelection();
      });
      toolbar.append(videoPicker);
    }
    toolbar.append(legend, reset);

    const frame = document.createElement("div");
    frame.className = "sd-network__frame";
    const tooltip = document.createElement("div");
    tooltip.className = "sd-network__tooltip";
    tooltip.setAttribute("role", "tooltip");
    const help = document.createElement("p");
    help.className = "sd-network__help";
    help.textContent = "Hover for details, click to isolate connections, drag nodes, or zoom and pan.";
    element.append(toolbar, frame, tooltip, help);

    if (nodes.length === 0 || links.length === 0) {
      frame.textContent = "No network edges matched the selected scope.";
      return;
    }

    const svg = global.d3.select(frame)
      .append("svg")
      .attr("viewBox", [0, 0, width, height])
      .attr("role", "img")
      .attr("aria-label", "Interactive chatter, video, and stream-title classification network");
    const viewport = svg.append("g");

    const maxNodeWeight = global.d3.max(nodes, (node) => Number(node.weight) || 0) || 1;
    const nodeRadius = global.d3.scaleSqrt().domain([0, maxNodeWeight]).range([4, 18]);
    const engagementMax = global.d3.max(
      links.filter((item) => item.type === "engagement"),
      (item) => Number(item.weight) || 0
    ) || 1;
    const classificationMax = global.d3.max(
      links.filter((item) => item.type === "classification"),
      (item) => Number(item.weight) || 0
    ) || 1;
    const engagementWidth = global.d3.scaleSqrt().domain([0, engagementMax]).range([0.8, 7]);
    const classificationWidth = global.d3.scaleSqrt().domain([0, classificationMax]).range([0.8, 8]);

    viewport.append("g")
      .selectAll("text")
      .data(Array.from(presentTypes))
      .join("text")
      .attr("class", "sd-network__column-label")
      .attr("x", (type) => TYPE_X[type])
      .attr("y", 28)
      .attr("text-anchor", "middle")
      .text((type) => TYPE_HEADINGS[type]);

    const link = viewport.append("g")
      .selectAll("line")
      .data(links)
      .join("line")
      .attr("class", "sd-network__link")
      .attr("stroke", (item) => item.type === "classification" ? "#8b5cf6" : "#60a5fa")
      .attr("stroke-opacity", (item) => item.type === "classification" ? 0.42 : 0.3)
      .attr("stroke-width", (item) => item.type === "classification"
        ? classificationWidth(Number(item.weight) || 0)
        : engagementWidth(Number(item.weight) || 0));

    const node = viewport.append("g")
      .selectAll("circle")
      .data(nodes)
      .join("circle")
      .attr("class", "sd-network__node")
      .attr("r", (item) => nodeRadius(Number(item.weight) || 0))
      .attr("fill", (item) => TYPE_COLORS[item.type] || "#64748b")
      .attr("tabindex", 0)
      .attr("aria-label", (item) => `${TYPE_LABELS[item.type] || item.type}: ${item.label}`);

    const label = viewport.append("g")
      .selectAll("text")
      .data(nodes.filter((item) => item.showLabel))
      .join("text")
      .attr("class", "sd-network__label")
      .attr("text-anchor", (item) => item.type === "Streamer" ? "end" : "start")
      .text((item) => item.label);

    const linkedIds = new Map(nodes.map((item) => [item.id, new Set([item.id])]));
    links.forEach((item) => {
      linkedIds.get(String(item.source))?.add(String(item.target));
      linkedIds.get(String(item.target))?.add(String(item.source));
    });

    function linkId(value) {
      return typeof value === "object" ? value.id : String(value);
    }

    function videoIdForLink(item) {
      return item.type === "classification" || item.type === "ownership"
        ? linkId(item.source)
        : linkId(item.target);
    }

    let selectedNode = null;

    function focusNode(item) {
      const neighbors = item ? linkedIds.get(item.id) : null;
      node.classed("sd-network__dimmed", (candidate) => neighbors && !neighbors.has(candidate.id));
      label.classed("sd-network__dimmed", (candidate) => neighbors && !neighbors.has(candidate.id));
      link.classed("sd-network__dimmed", (candidate) => neighbors && !(neighbors.has(linkId(candidate.source)) && neighbors.has(linkId(candidate.target))));
    }

    function applyVideoSelection() {
      if (!node || !link || !label) return;
      const visibleLinks = new Set();
      const visibleNodes = new Set();
      links.forEach((item) => {
        if (selectedVideoIds.has(videoIdForLink(item))) {
          visibleLinks.add(item);
          visibleNodes.add(linkId(item.source));
          visibleNodes.add(linkId(item.target));
        }
      });
      node.style("display", (item) => visibleNodes.has(item.id) ? null : "none");
      label.style("display", (item) => visibleNodes.has(item.id) ? null : "none");
      link.style("display", (item) => visibleLinks.has(item) ? null : "none");
      selectedNode = null;
      focusNode(null);
    }

    function showTooltip(event, item) {
      if (item.tooltip) {
        tooltip.textContent = item.tooltip;
        tooltip.style.left = `${event.offsetX}px`;
        tooltip.style.top = `${event.offsetY}px`;
        tooltip.style.visibility = "visible";
        return;
      }
      const weight = Number(item.weight || 0).toLocaleString();
      const typeLabel = TYPE_LABELS[item.type] || item.type;
      tooltip.textContent = `${item.label} · ${typeLabel} · weight ${weight}`;
      tooltip.style.left = `${event.offsetX}px`;
      tooltip.style.top = `${event.offsetY}px`;
      tooltip.style.visibility = "visible";
    }

    function hideTooltip() {
      tooltip.style.visibility = "hidden";
    }

    node
      .on("mouseenter", function (event, item) {
        focusNode(item);
        showTooltip(event, item);
      })
      .on("mousemove", showTooltip)
      .on("mouseleave", function () {
        focusNode(selectedNode);
        hideTooltip();
      })
      .on("click", function (event, item) {
        event.stopPropagation();
        selectedNode = selectedNode?.id === item.id ? null : item;
        focusNode(selectedNode);
      })
      .on("keydown", function (event, item) {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          selectedNode = selectedNode?.id === item.id ? null : item;
          focusNode(selectedNode);
        }
      });

    link
      .on("mouseenter", showTooltip)
      .on("mousemove", showTooltip)
      .on("mouseleave", hideTooltip);

    const simulation = global.d3.forceSimulation(nodes)
      .force("link", global.d3.forceLink(links).id((item) => item.id).distance((item) => item.type === "classification" ? 250 : 330).strength(0.16))
      .force("x", global.d3.forceX((item) => TYPE_X[item.type] || width / 2).strength(0.92))
      .force("y", global.d3.forceY(height / 2).strength(0.045))
      .force("charge", global.d3.forceManyBody().strength(-24))
      .force("collision", global.d3.forceCollide((item) => nodeRadius(Number(item.weight) || 0) + 3))
      .alphaDecay(0.035)
      .on("tick", function () {
        nodes.forEach((item) => {
          const radius = nodeRadius(Number(item.weight) || 0);
          item.x = Math.max(radius + 18, Math.min(width - radius - 18, item.x));
          item.y = Math.max(radius + 46, Math.min(height - radius - 18, item.y));
        });
        link
          .attr("x1", (item) => item.source.x)
          .attr("y1", (item) => item.source.y)
          .attr("x2", (item) => item.target.x)
          .attr("y2", (item) => item.target.y);
        node.attr("cx", (item) => item.x).attr("cy", (item) => item.y);
        label
          .attr("x", (item) => item.x + (item.type === "Streamer" ? -nodeRadius(item.weight) - 5 : nodeRadius(item.weight) + 5))
          .attr("y", (item) => item.y + 4);
      });
    element.__sdSimulation = simulation;

    node.call(global.d3.drag()
      .on("start", function (event, item) {
        if (!event.active) simulation.alphaTarget(0.2).restart();
        item.fx = item.x;
        item.fy = item.y;
      })
      .on("drag", function (event, item) {
        item.fx = event.x;
        item.fy = event.y;
      })
      .on("end", function (event, item) {
        if (!event.active) simulation.alphaTarget(0);
        item.fx = null;
        item.fy = null;
      }));

    const zoom = global.d3.zoom()
      .scaleExtent([0.45, 5])
      .on("zoom", (event) => viewport.attr("transform", event.transform));
    svg.call(zoom).on("dblclick.zoom", null).on("click", function () {
      selectedNode = null;
      focusNode(null);
    });

    search.addEventListener("input", function () {
      const query = search.value.trim().toLocaleLowerCase();
      if (!query) {
        focusNode(selectedNode);
        return;
      }
      const matches = new Set(nodes
        .filter((item) => String(item.label).toLocaleLowerCase().includes(query))
        .map((item) => item.id));
      node.classed("sd-network__dimmed", (item) => !matches.has(item.id));
      label.classed("sd-network__dimmed", (item) => !matches.has(item.id));
      link.classed("sd-network__dimmed", true);
    });

    reset.addEventListener("click", function () {
      search.value = "";
      selectedNode = null;
      focusNode(null);
      svg.transition().duration(250).call(zoom.transform, global.d3.zoomIdentity);
    });
  }

  global.SunDataNetwork = Object.assign(global.SunDataNetwork || {}, {
    renderViewerActivityNetwork
  });
}(window));
