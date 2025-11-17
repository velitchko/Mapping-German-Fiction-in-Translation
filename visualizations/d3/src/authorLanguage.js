import { createTooltip, showTooltip, hideTooltip, formatNumber, filterByWeight } from "./utils.js";

export function renderAuthorLanguageNetwork(containerSelector, data, options = {}) {
  const { minWeight = 5 } = options;
  const filtered = filterByWeight(data, minWeight);

  const nodes = filtered.nodes.map((node) => ({ ...node }));
  const links = filtered.links.map((link) => ({ ...link }));

  const container = d3.select(containerSelector);
  container.selectAll("*").remove();
  const { width } = container.node().getBoundingClientRect();
  const height = Math.max(520, width * 0.6);

  const svg = container.append("svg").attr("viewBox", [0, 0, width, height]);
  const tooltip = createTooltip();

  const maxNodeWeight = d3.max(nodes, (d) => d.totalWeight) || 1;
  const radiusScale = d3
    .scaleSqrt()
    .domain([1, maxNodeWeight])
    .range([4, 24]);

  const linkWidthScale = d3
    .scaleLinear()
    .domain(d3.extent(links, (d) => d.weight))
    .range([0.5, 5]);

  const colorScale = d3.scaleOrdinal()
    .domain(["author", "language"])
    .range(["#f97316", "#0ea5e9"]);

  const simulation = d3
    .forceSimulation(nodes)
    .force(
      "link",
      d3
        .forceLink(links)
        .id((d) => d.id)
        .distance((d) => 220 - Math.min(d.weight * 4, 150))
        .strength((d) => Math.min(0.2 + d.weight / 100, 0.8))
    )
    .force("charge", d3.forceManyBody().strength(-90))
    .force("center", d3.forceCenter(width / 2, height / 2))
    .force("collision", d3.forceCollide().radius((d) => radiusScale(d.totalWeight) + 6));

  const link = svg
    .append("g")
    .attr("stroke", "#1e3a8a")
    .attr("stroke-opacity", 0.3)
    .selectAll("line")
    .data(links)
    .join("line")
    .attr("class", "link")
    .attr("stroke-width", (d) => linkWidthScale(d.weight));

  link
    .on("mouseover", (event, d) => {
      showTooltip(
        tooltip,
        `<strong>${d.source.id} <-> ${d.target.id}</strong><br/>Translations: ${d.weight}`,
        event
      );
    })
    .on("mouseout", () => hideTooltip(tooltip));

  const node = svg
    .append("g")
    .selectAll("circle")
    .data(nodes)
    .join("circle")
    .attr("class", "node")
    .attr("r", (d) => radiusScale(d.totalWeight))
    .attr("fill", (d) => colorScale(d.type))
    .call(
      d3
        .drag()
        .on("start", (event, d) => {
          if (!event.active) simulation.alphaTarget(0.3).restart();
          d.fx = d.x;
          d.fy = d.y;
        })
        .on("drag", (event, d) => {
          d.fx = event.x;
          d.fy = event.y;
        })
        .on("end", (event, d) => {
          if (!event.active) simulation.alphaTarget(0);
          d.fx = null;
          d.fy = null;
        })
    );

  node
    .on("mouseover", (event, d) => {
      showTooltip(
        tooltip,
        `<strong>${d.label}</strong><br/>Type: ${d.type}<br/>Translations: ${formatNumber(
          d.totalWeight
        )}<br/>Connected languages: ${d.linkCount}`,
        event
      );
    })
    .on("mouseout", () => hideTooltip(tooltip));

  const label = svg
    .append("g")
    .selectAll("text")
    .data(nodes)
    .join("text")
    .text((d) => d.label)
    .attr("font-size", 10)
    .attr("fill", "#111827")
    .attr("text-anchor", "middle");

  simulation.on("tick", () => {
    link
      .attr("x1", (d) => d.source.x)
      .attr("y1", (d) => d.source.y)
      .attr("x2", (d) => d.target.x)
      .attr("y2", (d) => d.target.y);

    node.attr("cx", (d) => d.x).attr("cy", (d) => d.y);

    label.attr("x", (d) => d.x).attr("y", (d) => d.y - radiusScale(d.totalWeight) - 3);
  });

  // Add a simple legend
  const legend = svg.append("g").attr("class", "legend").attr("transform", `translate(15, 20)`);
  [
    { label: "Author", color: "#f97316", y: 0 },
    { label: "Language", color: "#0ea5e9", y: 20 },
  ].forEach((entry) => {
    legend.append("circle").attr("cx", 0).attr("cy", entry.y).attr("r", 6).attr("fill", entry.color);
    legend
      .append("text")
      .attr("x", 12)
      .attr("y", entry.y + 4)
      .text(entry.label);
  });

  return {
    nodes: filtered.nodes,
    links: filtered.links,
  };
}
