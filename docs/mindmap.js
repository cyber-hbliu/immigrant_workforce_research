/* Full-screen framework figure. Click a box for its finding and outputs, click an arrow for how the step it leads to was done.
   Draws FW (framework.js). Charts in the popover come from results.json. */
(function () {
  const C = { teal: "#00b0be", tealDark: "#0d7d87", orange: "#ea801c", gold: "#c99b38", grey: "#a1a1a1", ink: "#0d0d0d", grid: "#ececec" };
  const wrap = d3.select("#mindmap"), pop = d3.select("#mm-pop");
  const svg = wrap.append("svg").attr("viewBox", `0 0 ${FW.W} ${FW.H}`).attr("preserveAspectRatio", "xMidYMid meet");
  const defs = svg.append("defs");
  [["ink", "#0d0d0d"], ["grey", "#8a8a8a"], ["main", "#0d7d87"]].forEach(([k, c]) =>
    defs.append("marker").attr("id", "ar-" + k).attr("viewBox", "0 0 10 10").attr("refX", 9).attr("refY", 5).attr("markerWidth", 7).attr("markerHeight", 7).attr("orient", "auto-start-reverse")
      .append("path").attr("d", "M0,0 L10,5 L0,10 z").attr("fill", c));
  const g = svg.append("g");
  const ORDER = ["data", "sample", "eq", "q1", "q2", "q3", "nat", "own", "mig", "app"];
  const byId = Object.fromEntries(FW.boxes.map(b => [b.id, b]));
  let R = null;
  const PAN = {"p-area": "ch-map", "p-calc": "ch-enclave", "p-cohort": "ch-cohort", "p-coef": "ch-compare", "p-moves": "ch-moves", "p-occ": "ch-occ", "p-flow": "ch-globe", "p-tables": "tables", "p-gap": "ch-slopes", "p-wage": "ch-slopes", "p-cells": "ch-grid", "p-pop": "ch-globe", "p-county": "ch-county"};

  FW.headings.forEach(h => g.append("text").attr("class", "fw-h").attr("x", h.x).attr("y", h.y).text(h.text));
  FW.labels.forEach(l => g.append("text").attr("class", l.cls).attr("x", l.x).attr("y", l.y).attr("text-anchor", l.anchor || "middle").text(l.text));
  g.append("line").attr("class", "fw-rule").attr("x1", 80).attr("x2", FW.W - 80).attr("y1", FW.rule.y).attr("y2", FW.rule.y);
  FW.legend.forEach(l => {
    g.append("rect").attr("class", "fw-box " + l.style).attr("x", l.x).attr("y", l.y - 22).attr("width", 76).attr("height", 44).attr("rx", 10);
    g.append("text").attr("class", "fw-line").attr("x", l.x + 96).attr("y", l.y + 9).text(l.text);
  });
  g.append("text").attr("class", "fw-small").attr("x", FW.W - 80).attr("y", 60).attr("text-anchor", "end").style("fill", "#7f7f7f").text("click a box for its finding · click an arrow for the method");

  // arrows: each points at a box; the arrow opens that box's method
  const arrowTarget = ["Y", "Y", "sample", "eq", "q1", "q3", "q2", "nat", "own", "own"];
  const arrowFrom = ["D", "L", "data", "sample", "eq", "eq", "q1", "q1", "q2", "mig"];
  const arrows = FW.arrows.map((a, i) => ({ ...a, tgt: arrowTarget[i], src: arrowFrom[i], d: a.path || `M${a.from[0]},${a.from[1]} L${a.to[0]},${a.to[1]}` }));
  const gA = g.append("g");
  const hits = gA.selectAll("g.arr").data(arrows).join("g").attr("class", "arr");
  hits.append("path").attr("class", "fw-hit").attr("d", d => d.d);
  hits.append("path").attr("class", d => "fw-arrow " + d.style).attr("d", d => d.d).attr("marker-end", d => `url(#ar-${d.style})`);

  const box = g.selectAll("g.fw-node").data(FW.boxes).join("g").attr("class", d => "fw-node " + d.style).attr("transform", d => `translate(${d.x},${d.y})`);
  box.append("rect").attr("class", d => "fw-box " + d.style).attr("width", d => d.w).attr("height", d => d.h).attr("rx", 14);
  box.append("text").attr("class", d => "fw-title" + (d.small ? " small" : "")).attr("x", 22).attr("y", d => d.small ? d.h / 2 + 10 : 46).text(d => d.title);
  box.each(function (d) {
    const t = d3.select(this);
    (d.lines || []).forEach((ln, i) => t.append("text").attr("class", "fw-line").attr("x", 22).attr("y", 96 + i * 38).text(ln));
    if (d.ref) t.append("text").attr("class", "fw-ref").attr("x", 22).attr("y", d.lines ? d.h - 24 : 96).text(d.ref);
    const n = ORDER.indexOf(d.id);
    if (n >= 0) { const b = t.append("g").attr("class", "fw-step").attr("transform", `translate(${d.w - 4},4)`); b.append("circle").attr("r", 20); b.append("text").attr("dy", 8).attr("text-anchor", "middle").text(n + 1); }
  });

  // ---------- mini charts (same as before) ----------
  const fmt3 = d3.format("+.3f");
  function frame(el, h = 160) { const w = el.node().getBoundingClientRect().width || 340; const s = el.append("svg").attr("class", "mini").attr("viewBox", `0 0 ${w} ${h}`).attr("width", "100%"); return { s, w, h }; }
  function hbars(el, rows, opts = {}) {
    const { s, w } = frame(el, rows.length * 28 + 20), m = { l: opts.l || 150, r: 60 }, cw = w - m.l - m.r;
    const x = d3.scaleLinear().domain(opts.domain || [Math.min(0, d3.min(rows, d => d.v)), Math.max(0, d3.max(rows, d => d.v))]).range([0, cw]);
    const gg = s.append("g").attr("transform", `translate(${m.l},6)`);
    gg.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("y1", 0).attr("y2", rows.length * 28).attr("stroke", C.ink);
    const r = gg.selectAll("g").data(rows).join("g").attr("transform", (d, i) => `translate(0,${i * 28})`);
    r.append("text").attr("x", -8).attr("y", 15).attr("text-anchor", "end").text(d => d.k);
    r.append("rect").attr("x", d => x(Math.min(0, d.v))).attr("y", 4).attr("height", 17).attr("width", d => Math.abs(x(d.v) - x(0))).attr("fill", d => d.c || C.teal).attr("rx", 2);
    if (opts.se) r.append("line").attr("x1", d => x(d.v - 2 * d.se)).attr("x2", d => x(d.v + 2 * d.se)).attr("y1", 12.5).attr("y2", 12.5).attr("stroke", C.ink).attr("stroke-width", 1.5);
    r.append("text").attr("x", d => (d.v >= 0 ? x(Math.max(d.v, 0) + (opts.se ? 2 * d.se : 0)) : x(0)) + 6).attr("y", 15).attr("class", "label").text(d => opts.f ? opts.f(d) : d.v);
  }
  function lines2(el, spec) {
    const { s, w, h } = frame(el, 170), m = { l: 44, r: 16, t: 10, b: 26 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const x = d3.scaleLinear().domain([-1.5, 2.5]).range([0, cw]), y = d3.scaleLinear().domain([-0.5, 0.15]).range([ch, 0]);
    const gg = s.append("g").attr("transform", `translate(${m.l},${m.t})`);
    gg.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).ticks(4).tickFormat(d => d + " SD").tickSizeOuter(0));
    gg.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(4).tickFormat(d3.format("+.1f")).tickSizeOuter(0));
    const pts = d3.range(-1.5, 2.51, 0.25);
    [[spec.prof, 0, C.teal, "proficient"], [spec.lim, spec.gap, C.orange, "limited English"]].forEach(([b, a, c, n]) => {
      gg.append("path").datum(pts).attr("d", d3.line().x(d => x(d)).y(d => y(a + b * d))).attr("fill", "none").attr("stroke", c).attr("stroke-width", 2.5);
      gg.append("text").attr("x", x(2.5) - 4).attr("y", y(a + b * 2.5) - 8).attr("text-anchor", "end").attr("fill", c).text(`${n} ${fmt3(b)}/SD`);
    });
  }
  function trend(el) {
    const { s, w, h } = frame(el, 140), m = { l: 44, r: 16, t: 10, b: 24 }, cw = w - m.l - m.r, ch = h - m.t - m.b, tr = R.trend;
    const x = d3.scaleLinear().domain([2013, 2024]).range([0, cw]), y = d3.scaleLinear().domain([180000, 245000]).range([ch, 0]);
    const gg = s.append("g").attr("transform", `translate(${m.l},${m.t})`);
    gg.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).ticks(4).tickFormat(d3.format("d")).tickSizeOuter(0));
    gg.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(3).tickFormat(d => d / 1000 + "k").tickSizeOuter(0));
    gg.append("path").datum(tr).attr("d", d3.line().x(d => x(d.year)).y(d => y(d.foreign_born)).curve(d3.curveMonotoneX)).attr("fill", "none").attr("stroke", C.teal).attr("stroke-width", 2.5);
  }
  function stacked(el) {
    const O = R.results.oaxaca, { s, w, h } = frame(el, 140), m = { l: 44, r: 16, t: 10, b: 24 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const x = d3.scaleBand().domain(["without English", "with English"]).range([0, cw]).padding(0.45), y = d3.scaleLinear().domain([0, 0.12]).range([ch, 0]);
    const gg = s.append("g").attr("transform", `translate(${m.l},${m.t})`);
    gg.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).tickSizeOuter(0));
    gg.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(3).tickSizeOuter(0));
    [["without English", O.base], ["with English", O.with_english]].forEach(([n, v]) => {
      gg.append("rect").attr("x", x(n)).attr("y", y(v.explained)).attr("width", x.bandwidth()).attr("height", ch - y(v.explained)).attr("fill", C.teal);
      gg.append("rect").attr("x", x(n)).attr("y", y(v.explained + v.unexplained)).attr("width", x.bandwidth()).attr("height", y(v.explained) - y(v.explained + v.unexplained)).attr("fill", C.orange);
      gg.append("text").attr("x", x(n) + x.bandwidth() / 2).attr("y", y(v.explained + v.unexplained / 2) + 4).attr("text-anchor", "middle").attr("fill", "#fff").text("unexplained " + v.unexplained.toFixed(3));
    });
  }
  const CHART = {
    D: el => hbars(el, R.results.pumas_top.density.map(d => ({ k: d[0].replace(/ \(.*\)/, "").replace("County ", ""), v: d[1] })), { l: 190, f: d => d.v + "%" }),
    L: el => hbars(el, R.results.t1.map(d => ({ k: d.eng, v: d.median, c: C.gold })), { l: 90, f: d => "$" + d.v.toFixed(2) }),
    Y: el => lines2(el, { prof: R.results.enclave.A.density, lim: R.results.enclave.A.density + R.results.enclave.A.inter, gap: R.results.enclave.A.limited }),
    alt: el => hbars(el, [{ k: "proficient immigrants", v: -0.084, se: 0.018 }, { k: "limited-English immigrants", v: -0.034, se: 0.020 }, { k: "native-born", v: -0.020, se: 0.011, c: C.grey }, { k: "immigrants, within occupation", v: -0.048, se: 0.013, c: C.tealDark }], { l: 190, se: true, f: d => fmt3(d.v), domain: [-0.13, 0.02] }),
    data: trend,
    sample: el => hbars(el, [{ k: "all workers", v: 240089, c: C.grey }, { k: "foreign-born", v: 29299 }, { k: "native-born 2022–24", v: 47893, c: C.grey }, { k: "foreign-born 2022–24", v: 7355 }, { k: "non-English language", v: 5732, c: C.tealDark }], { l: 150, f: d => d3.format(",")(d.v) }),
    eq: el => hbars(el, [["before 1990", -0.243], ["1990s", -0.240], ["2000s", -0.196], ["2010s", -0.184], ["2020 onward", -0.190]].map(d => ({ k: d[0], v: d[1], c: C.gold })), { l: 100, f: d => fmt3(d.v), domain: [-0.3, 0] }),
    q1: el => lines2(el, { prof: R.results.enclave.A.density, lim: R.results.enclave.A.density + R.results.enclave.A.inter, gap: R.results.enclave.A.limited }),
    q2: el => hbars(el, [{ k: "limited English", v: -0.234, se: 0.033 }, { k: "same, within occupation", v: -0.092, se: 0.029, c: C.tealDark }, { k: "limited × density", v: 0.049, se: 0.029, c: C.orange }, { k: "same, within occupation", v: 0.007, se: 0.016, c: "#f3b47a" }], { l: 170, se: true, f: d => fmt3(d.v), domain: [-0.3, 0.12] }),
    q3: el => hbars(el, R.results.rrr.map(d => ({ k: d.move.replace(/_/g, " "), v: Math.log(d.rrr), lo: d.lo, hi: d.hi, c: d.move === "city_to_suburb" ? C.orange : C.grey })), { l: 130, f: d => `${Math.exp(d.v).toFixed(2)} (${d.lo} to ${d.hi})`, domain: [-0.5, 0.5] }),
    nat: el => hbars(el, R.results.slopes.generic.map(d => ({ k: d.g.toLowerCase(), v: d.v, se: d.se, c: d.g.startsWith("Native") ? C.grey : C.teal })).concat([{ k: "proficient, native wage control", v: -0.064, se: 0.013, c: C.tealDark }]), { l: 190, se: true, f: d => fmt3(d.v), domain: [-0.13, 0.02] }),
    own: el => { const O = R.results.ownlang; hbars(el, [{ k: "Philadelphia, interaction", v: O.Philadelphia.inter, se: O.Philadelphia.inter_se }, { k: "Philadelphia, within occ.", v: 0.021, se: 0.013, c: C.tealDark }, { k: "New York, interaction", v: O["New York"].inter, se: O["New York"].inter_se, c: C.orange }, { k: "New York, within occ.", v: 0.028, se: 0.007, c: "#f3b47a" }], { l: 170, se: true, f: d => fmt3(d.v), domain: [-0.02, 0.1] }); },
    mig: el => hbars(el, [{ k: "left a concentrated area", v: -0.059, se: 0.177 }, { k: "× limited English", v: -0.013, se: 0.162, c: C.orange }], { l: 170, se: true, f: d => `${fmt3(d.v)} (${d.se})`, domain: [-0.5, 0.5] }),
    app: stacked
  };

  // ---------- popover ----------
  function placeAt(ev) {
    const r = wrap.node().getBoundingClientRect(), pw = 380;
    let x = ev.clientX - r.left + 16, y = ev.clientY - r.top - 20;
    if (x + pw > r.width - 10) x = ev.clientX - r.left - pw - 16;
    if (x < 10) x = 10;
    pop.style("left", x + "px").style("top", Math.max(10, y) + "px").attr("hidden", null);
    requestAnimationFrame(() => { const ph = pop.node().offsetHeight; if (y + ph > r.height - 10) pop.style("top", Math.max(10, r.height - ph - 10) + "px"); });
  }
  function showBox(d, ev) {
    const n = ORDER.indexOf(d.id), outs = (d.outputs || []).map(o => `<span class="chip">${o}</span>`).join("");
    pop.html(`<button class="close" aria-label="close">×</button><div class="mm-kicker">${n >= 0 ? `Step ${n + 1} · ` : ""}${d.kicker || ""} · finding</div><h2>${d.title}</h2><p class="mm-summary">${d.summary || ""}</p>
      <div class="mm-chart"></div>${d.finding ? `<p class="mm-finding">${d.finding}</p>` : ""}
      <div class="mm-actions"><a class="primary" href="#story/ch-${d.story}">Read it in the story</a><a href="#story/${PAN[d.panel] || "ch-" + d.story}">Open the chart</a></div>`);
    if (R && CHART[d.id]) CHART[d.id](pop.select(".mm-chart"));
    box.classed("on", b => b === d); hits.select(".fw-arrow").classed("on", false);
    placeAt(ev); pop.select(".close").on("click", hide);
  }
  function showArrow(a, ev) {
    const d = byId[a.tgt], from = byId[a.src], n = ORDER.indexOf(d.id), steps = (d.steps || []).map(s => `<li>${s}</li>`).join("");
    pop.html(`<button class="close" aria-label="close">×</button><div class="mm-kicker">${n >= 0 ? `Step ${n + 1} · ` : ""}method</div><h2>${from ? from.title + " → " : ""}${d.title}</h2><p class="mm-summary">${d.summary || ""}</p>
      ${steps ? `<h3>How it was done</h3><ol>${steps}</ol>` : ""}
      <div class="mm-actions"><a href="#story/${PAN[d.panel] || "ch-" + d.story}">Open the chart</a></div>`);
    hits.select(".fw-arrow").classed("on", x => x === a); box.classed("on", false);
    placeAt(ev); pop.select(".close").on("click", hide);
  }
  function hide() { pop.attr("hidden", true); box.classed("on", false); hits.select(".fw-arrow").classed("on", false); }
  box.on("click", (ev, d) => { ev.stopPropagation(); showBox(d, ev); });
  hits.style("cursor", "pointer").on("click", (ev, a) => { ev.stopPropagation(); showArrow(a, ev); });
  svg.on("click", hide);
  document.addEventListener("keydown", ev => { if (ev.key === "Escape") hide(); });

  Promise.all([d3.json("data/results.json"), d3.csv("data/trend.csv")]).then(([results, tr]) => { R = { results, trend: tr.map(d => ({ year: +d.year, foreign_born: +d.foreign_born })) }; });
})();
