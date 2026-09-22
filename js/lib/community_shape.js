(function (global) {
  "use strict";

  const SEGMENT_COLORS = {
    "Drop-in": "#94a3b8",
    "Returning": "#38bdf8",
    "Regular": "#6366f1",
    "Core": "#8b5cf6"
  };

  function createSurface(element, width, height, ariaLabel) {
    if (!element) throw new Error("A chart container element is required.");
    if (!global.d3) throw new Error("D3 must be loaded before the community renderer.");
    element.replaceChildren();
    element.classList.add("sd-community-chart");
    const svg = global.d3.select(element)
      .append("svg")
      .attr("viewBox", [0, 0, width, height])
      .attr("role", "img")
      .attr("aria-label", ariaLabel);
    const tooltip = document.createElement("div");
    tooltip.className = "sd-community-chart__tooltip";
    tooltip.setAttribute("role", "tooltip");
    element.append(tooltip);
    return { svg, tooltip };
  }

  function showTooltip(tooltip, event, lines) {
    tooltip.replaceChildren();
    lines.forEach((line, index) => {
      const row = document.createElement("div");
      if (index === 0) row.className = "sd-community-chart__tooltip-title";
      row.textContent = line;
      tooltip.append(row);
    });
    tooltip.style.left = `${event.offsetX}px`;
    tooltip.style.top = `${event.offsetY}px`;
    tooltip.style.visibility = "visible";
  }

  function hideTooltip(tooltip) {
    tooltip.style.visibility = "hidden";
  }

  function compactNumber(value) {
    return new Intl.NumberFormat(undefined, { notation: "compact", maximumFractionDigits: 1 }).format(value);
  }

  function percent(value) {
    return new Intl.NumberFormat(undefined, { style: "percent", maximumFractionDigits: 1 }).format(value);
  }

  function truncate(value, length) {
    const text = String(value || "Untitled video");
    return text.length > length ? `${text.slice(0, length - 1)}…` : text;
  }

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
        ? `Compare: all ${videos.length} videos`
        : `Compare: ${selected.size} of ${videos.length} videos`;
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

  function renderSegments(element, data, options) {
    const settings = Object.assign({ height: 310 }, options || {});
    const width = 1000;
    const height = Number(settings.height) || 310;
    const rows = (data.rows || []).slice().sort((a, b) => Number(a.order) - Number(b.order));
    const { svg, tooltip } = createSurface(
      element,
      width,
      height,
      "Community layers showing chatter counts and message share"
    );
    const margin = { top: 20, right: 190, bottom: 52, left: 125 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;
    const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
    const maximum = global.d3.max(rows, (row) => Number(row.chatters)) || 1;
    const x = global.d3.scaleLinear().domain([0, maximum]).nice().range([0, innerWidth]);
    const y = global.d3.scaleBand()
      .domain(rows.map((row) => row.segment))
      .range([0, innerHeight])
      .padding(0.24);

    chart.append("g")
      .attr("class", "sd-community-chart__grid")
      .call(global.d3.axisBottom(x).ticks(6).tickSize(innerHeight).tickFormat(""));
    chart.append("g")
      .attr("class", "sd-community-chart__axis")
      .call(global.d3.axisLeft(y).tickSize(0))
      .call((axis) => axis.select(".domain").remove());
    chart.append("g")
      .attr("class", "sd-community-chart__axis")
      .attr("transform", `translate(0,${innerHeight})`)
      .call(global.d3.axisBottom(x).ticks(6).tickFormat(compactNumber));

    chart.selectAll("rect.sd-community-chart__bar")
      .data(rows)
      .join("rect")
      .attr("class", "sd-community-chart__bar")
      .attr("x", 0)
      .attr("y", (row) => y(row.segment))
      .attr("width", (row) => x(Number(row.chatters)))
      .attr("height", y.bandwidth())
      .attr("rx", 5)
      .attr("fill", (row) => SEGMENT_COLORS[row.segment] || "#64748b")
      .on("mouseenter", function (event, row) {
        showTooltip(tooltip, event, [
          row.segment,
          `${Number(row.chatters).toLocaleString()} chatters (${percent(row.chatter_share)})`,
          `${Number(row.messages).toLocaleString()} messages (${percent(row.message_share)})`
        ]);
      })
      .on("mousemove", function (event, row) {
        showTooltip(tooltip, event, [
          row.segment,
          `${Number(row.chatters).toLocaleString()} chatters (${percent(row.chatter_share)})`,
          `${Number(row.messages).toLocaleString()} messages (${percent(row.message_share)})`
        ]);
      })
      .on("mouseleave", () => hideTooltip(tooltip));

    chart.selectAll("text.sd-community-chart__bar-label")
      .data(rows)
      .join("text")
      .attr("class", "sd-community-chart__bar-label")
      .attr("x", (row) => x(Number(row.chatters)) + 9)
      .attr("y", (row) => y(row.segment) + y.bandwidth() / 2 + 4)
      .text((row) => `${Number(row.chatters).toLocaleString()} · ${percent(row.message_share)} of messages`);

    svg.append("text")
      .attr("class", "sd-community-chart__axis-title")
      .attr("x", margin.left + innerWidth / 2)
      .attr("y", height - 7)
      .attr("text-anchor", "middle")
      .text("Number of chatters");
  }

  function renderLandscape(element, data, options) {
    const settings = Object.assign({ height: 430 }, options || {});
    const width = 900;
    const height = Number(settings.height) || 430;
    const cells = data.cells || [];
    const breadth = data.breadth || [];
    const intensity = (data.intensity || []).slice().reverse();
    const { svg, tooltip } = createSurface(
      element,
      width,
      height,
      "Engagement landscape showing chatter counts by stream breadth and message intensity"
    );
    const margin = { top: 20, right: 100, bottom: 75, left: 120 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;
    const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
    const x = global.d3.scaleBand().domain(breadth).range([0, innerWidth]).padding(0.07);
    const y = global.d3.scaleBand().domain(intensity).range([0, innerHeight]).padding(0.07);
    const maximum = global.d3.max(cells, (cell) => Number(cell.chatters)) || 1;
    const color = global.d3.scaleSequentialSqrt([0, maximum], global.d3.interpolateBlues);

    chart.selectAll("rect")
      .data(cells)
      .join("rect")
      .attr("x", (cell) => x(cell.breadth_band))
      .attr("y", (cell) => y(cell.intensity_band))
      .attr("width", x.bandwidth())
      .attr("height", y.bandwidth())
      .attr("rx", 5)
      .attr("fill", (cell) => Number(cell.chatters) === 0 ? "#f1f5f9" : color(Number(cell.chatters)))
      .on("mouseenter", function (event, cell) {
        showTooltip(tooltip, event, [
          `${cell.breadth_band} stream(s) · ${cell.intensity_band} messages`,
          `${Number(cell.chatters).toLocaleString()} chatters`
        ]);
      })
      .on("mousemove", function (event, cell) {
        showTooltip(tooltip, event, [
          `${cell.breadth_band} stream(s) · ${cell.intensity_band} messages`,
          `${Number(cell.chatters).toLocaleString()} chatters`
        ]);
      })
      .on("mouseleave", () => hideTooltip(tooltip));

    chart.selectAll("text.sd-community-chart__cell-label")
      .data(cells.filter((cell) => Number(cell.chatters) > 0))
      .join("text")
      .attr("class", "sd-community-chart__cell-label")
      .attr("x", (cell) => x(cell.breadth_band) + x.bandwidth() / 2)
      .attr("y", (cell) => y(cell.intensity_band) + y.bandwidth() / 2 + 5)
      .attr("text-anchor", "middle")
      .classed("sd-community-chart__cell-label--light", (cell) => Number(cell.chatters) > maximum * 0.38)
      .text((cell) => Number(cell.chatters).toLocaleString());

    chart.append("g")
      .attr("class", "sd-community-chart__axis")
      .attr("transform", `translate(0,${innerHeight})`)
      .call(global.d3.axisBottom(x).tickSize(0))
      .call((axis) => axis.select(".domain").remove());
    chart.append("g")
      .attr("class", "sd-community-chart__axis")
      .call(global.d3.axisLeft(y).tickSize(0))
      .call((axis) => axis.select(".domain").remove());
    svg.append("text")
      .attr("class", "sd-community-chart__axis-title")
      .attr("x", margin.left + innerWidth / 2)
      .attr("y", height - 12)
      .attr("text-anchor", "middle")
      .text("Streams chatted in");
    svg.append("text")
      .attr("class", "sd-community-chart__axis-title")
      .attr("transform", "rotate(-90)")
      .attr("x", -(margin.top + innerHeight / 2))
      .attr("y", 19)
      .attr("text-anchor", "middle")
      .text("Total messages");
  }

  function renderOverlap(element, data, options) {
    const settings = Object.assign({ height: 720 }, options || {});
    const width = 1000;
    const height = Number(settings.height) || 720;
    const videos = (data.videos || []).slice().sort((a, b) => Number(a.order) - Number(b.order));
    const cells = data.cells || [];
    const ids = videos.map((video) => video.id);
    const videoById = new Map(videos.map((video) => [video.id, video]));
    const { svg, tooltip } = createSurface(
      element,
      width,
      height,
      "Stream audience-overlap matrix based on shared chatters"
    );
    let selectedVideoIds = new Set(ids);
    if (settings.showVideoPicker !== false) {
      const videoPicker = createVideoPicker(videos, function (selected) {
        selectedVideoIds = selected;
        applyVideoSelection();
      });
      element.insertBefore(videoPicker, element.firstChild);
    }
    const margin = { top: 24, right: 30, bottom: 220, left: 220 };
    const size = Math.min(width - margin.left - margin.right, height - margin.top - margin.bottom);
    const chart = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);
    const x = global.d3.scaleBand().domain(ids).range([0, size]).padding(0.035);
    const y = global.d3.scaleBand().domain(ids).range([0, size]).padding(0.035);
    const offDiagonal = cells.filter((cell) => cell.row_id !== cell.column_id);
    const maximum = global.d3.max(offDiagonal, (cell) => Number(cell.similarity)) || 1;
    const color = global.d3.scaleSequential([0, maximum], global.d3.interpolatePurples);

    const matrixCells = chart.selectAll("rect")
      .data(cells)
      .join("rect")
      .attr("x", (cell) => x(cell.column_id))
      .attr("y", (cell) => y(cell.row_id))
      .attr("width", x.bandwidth())
      .attr("height", y.bandwidth())
      .attr("rx", 3)
      .attr("fill", (cell) => cell.row_id === cell.column_id ? "#cbd5e1" : color(Number(cell.similarity)))
      .on("mouseenter", function (event, cell) {
        const row = videoById.get(cell.row_id);
        const column = videoById.get(cell.column_id);
        showTooltip(tooltip, event, [
          `${row?.label || cell.row_id} ↔ ${column?.label || cell.column_id}`,
          `${Number(cell.shared_chatters).toLocaleString()} shared chatters`,
          `${percent(cell.similarity)} audience similarity`
        ]);
      })
      .on("mousemove", function (event, cell) {
        const row = videoById.get(cell.row_id);
        const column = videoById.get(cell.column_id);
        showTooltip(tooltip, event, [
          `${row?.label || cell.row_id} ↔ ${column?.label || cell.column_id}`,
          `${Number(cell.shared_chatters).toLocaleString()} shared chatters`,
          `${percent(cell.similarity)} audience similarity`
        ]);
      })
      .on("mouseleave", () => hideTooltip(tooltip));

    const rowAxis = chart.append("g")
      .attr("class", "sd-community-chart__axis sd-community-chart__overlap-axis")
      .call(global.d3.axisLeft(y).tickSize(0).tickFormat((id) => truncate(videoById.get(id)?.label, 28)))
      .call((axis) => axis.select(".domain").remove());
    const columnAxis = chart.append("g")
      .attr("class", "sd-community-chart__axis sd-community-chart__overlap-axis")
      .attr("transform", `translate(0,${size})`)
      .call(global.d3.axisBottom(x).tickSize(0).tickFormat((id) => truncate(videoById.get(id)?.label, 28)))
      .call((axis) => axis.select(".domain").remove())
      .selectAll("text")
      .attr("text-anchor", "end")
      .attr("transform", "rotate(-48)")
      .attr("dx", "-0.65em")
      .attr("dy", "0.2em");

    function applyVideoSelection() {
      if (!matrixCells || !rowAxis || !columnAxis) return;
      matrixCells.classed(
        "sd-community-chart__cell--filtered",
        (cell) => !selectedVideoIds.has(cell.row_id) || !selectedVideoIds.has(cell.column_id)
      );
      rowAxis.selectAll(".tick").classed(
        "sd-community-chart__tick--filtered",
        (id) => !selectedVideoIds.has(id)
      );
      columnAxis.selectAll(".tick").classed(
        "sd-community-chart__tick--filtered",
        (id) => !selectedVideoIds.has(id)
      );
    }
  }

  global.SunDataCommunity = Object.assign(global.SunDataCommunity || {}, {
    renderSegments,
    renderLandscape,
    renderOverlap
  });
}(window));
