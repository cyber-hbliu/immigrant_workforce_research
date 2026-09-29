(function () {
/* Explore page. D3 v7. Reads data/results.json and, if present, data/pumas.geojson. */
const C = { ink: "#0d0d0d", ink2: "#595959", muted: "#7f7f7f", grid: "#ececec", axis: "#bababa",
  teal: "#00b0be", tealDark: "#0d7d87", tealPale: "#dff4f4", tealLight: "#8fd7d7", orange: "#ea801c", gold: "#c99b38", grey: "#a1a1a1", grey2: "#d4d4d4",
  region: { "Asia": "#00b0be", "Latin America": "#ea801c", "Europe": "#c99b38", "Africa": "#a1a1a1" } };
const fmt$ = d3.format("$,.0f"), fmtN = d3.format(","), T = 600, ease = d3.easeCubicInOut;
const tip = d3.select("body").append("div").attr("class", "tip").style("opacity", 0);
function hover(sel, html) {
  sel.on("mousemove", (ev, d) => tip.style("opacity", 1).html(html(d)).style("left", (ev.pageX + 14) + "px").style("top", (ev.pageY - 10) + "px"))
     .on("mouseleave", () => tip.style("opacity", 0));
}
function box(id) { const svg = d3.select(id); svg.selectAll("*").remove(); let { width, height } = svg.node().getBoundingClientRect(); if (!width) { width = 900; height = 480; } svg.attr("viewBox", `0 0 ${width} ${height}`); return { svg, w: width, h: height }; }
function axes(g, x, y, w, h, o = {}) {
  g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(o.yt || 5).tickSize(-w).tickFormat(""));
  g.append("g").attr("class", "axis").attr("transform", `translate(0,${h})`).call(d3.axisBottom(x).ticks(o.xt || 6).tickFormat(o.xf || null).tickSizeOuter(0));
  g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(o.yt || 5).tickFormat(o.yf || null).tickSizeOuter(0)).select(".domain").remove();
}
const R = {};

// ---------- 1. areas: linked map and scatter ----------
function areas() {
  const G = R.pumas;
  function detail(p) {
    if (!p) { d3.select("#d-area").html(`<h3>Hover or click an area</h3><p style="color:var(--muted)">Profiles appear here. Drag a box across the scatter to select several.</p>`); return; }
    d3.select("#d-area").html(`<h3>${p.name.replace(" PUMA", "")}</h3><div class="big">${(+p.puma_fb_prop).toFixed(1)}%</div><p style="margin:0 0 8px;color:var(--muted)">foreign-born</p>
      <dl><dt>limited English among foreign-born</dt><dd>${(+p.fb_limited_prop).toFixed(1)}%</dd><dt>foreign-born median wage</dt><dd>${fmt$(+p.fb_median_wage)}</dd><dt>location</dt><dd>${p.city ? "Philadelphia County" : "suburban county"}</dd></dl>`);
  }
  let chart;
  function draw() {
    const { svg, w, h } = box("#s-area");
    if (!G) { svg.append("text").attr("x", 20).attr("y", 40).attr("class", "title").text("Upload data/pumas.geojson for the map"); return; }
    chart = CH.linkedMap(svg, G, { width: w, height: h, stacked: true, mobility: R.mobility ? R.mobility.flows : null, onSelect: detail });
    chart.color(d3.select("#area-var").node().value); chart.trend(true);
    chart.showFlows(d3.select("#area-flows").node().value !== "none", d3.select("#area-flows").node().value);
  }
  d3.select("#area-var").on("change", () => chart && chart.color(d3.select("#area-var").node().value));
  d3.select("#area-flows").on("change", () => { const v = d3.select("#area-flows").node().value; chart && chart.showFlows(v !== "none", v); });
  detail(null); draw(); window.addEventListener("resize", draw);
}

// ---------- 2. differential calculator ----------
function calc() {
  const E = R.results.enclave, O = R.results.ownlang;
  const specs = {
    generic: { prof: E.A.density, lim: E.A.density + E.A.inter, gap: E.A.limited, sd: 5.4, name: "generic density, no occupation controls" },
    occ: { prof: E.B.density, lim: E.B.density + E.B.inter, gap: E.B.limited, sd: 5.4, name: "generic density, within occupations" },
    "own-phl": { prof: O.Philadelphia.prof, lim: O.Philadelphia.lim, gap: E.A.limited, sd: O.Philadelphia.sd_pp, name: "own-language density, Philadelphia" },
    "own-nyc": { prof: O["New York"].prof, lim: O["New York"].lim, gap: -0.2119, sd: O["New York"].sd_pp, name: "own-language density, New York" }
  };
  const sel = d3.select("#calc-measure"), sl = d3.select("#calc-d");
  function draw() {
    const S = specs[sel.node().value], d0 = +sl.node().value; d3.select("#calc-dv").text(`${d0 >= 0 ? "+" : ""}${d0.toFixed(1)} SD`);
    const { svg, w, h } = box("#s-calc"); const m = { t: 30, r: 30, b: 40, l: 60 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const x = d3.scaleLinear().domain([-1.5, 2.5]).range([0, cw]), y = d3.scaleLinear().domain([-0.5, 0.15]).range([ch, 0]);
    axes(g, x, y, cw, ch, { yf: d3.format("+.1f") });
    g.append("text").attr("x", cw / 2).attr("y", ch + 34).attr("text-anchor", "middle").text(`density, standard deviations from the mean (1 SD = ${S.sd} points)`);
    const pts = d3.range(-1.5, 2.51, 0.1), lp = d3.line().x(d => x(d)).y(d => y(S.prof * d)), ll = d3.line().x(d => x(d)).y(d => y(S.gap + S.lim * d));
    g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", C.axis);
    g.append("path").datum(pts).attr("d", lp).attr("fill", "none").attr("stroke", C.teal).attr("stroke-width", 2.6);
    g.append("path").datum(pts).attr("d", ll).attr("fill", "none").attr("stroke", C.orange).attr("stroke-width", 2.6);
    const yp = S.prof * d0, yl = S.gap + S.lim * d0;
    g.append("line").attr("x1", x(d0)).attr("x2", x(d0)).attr("y1", y(yp)).attr("y2", y(yl)).attr("stroke", C.ink).attr("stroke-dasharray", "3 3");
    g.append("circle").attr("cx", x(d0)).attr("cy", y(yp)).attr("r", 6).attr("fill", C.teal).attr("stroke", "#fff");
    g.append("circle").attr("cx", x(d0)).attr("cy", y(yl)).attr("r", 6).attr("fill", C.orange).attr("stroke", "#fff");
    g.append("text").attr("class", "label").attr("x", x(-1.5)).attr("y", y(S.prof * -1.5) - 10).attr("fill", C.tealDark).text("Proficient English");
    g.append("text").attr("class", "label").attr("x", x(-1.5)).attr("y", y(S.gap + S.lim * -1.5) - 10).attr("fill", C.orange).text("Limited English");
    const diff = yl - yp;
    d3.select("#d-calc").html(`<h3>${S.name}</h3><div class="big">${((1 - Math.exp(diff)) * 100).toFixed(0)}%</div>
      <p style="margin:0 0 8px;color:var(--muted)">limited-English differential at ${d0 >= 0 ? "+" : ""}${d0.toFixed(1)} SD of density</p>
      <dl><dt>proficient, log wage</dt><dd>${yp.toFixed(3)}</dd><dt>limited English, log wage</dt><dd>${yl.toFixed(3)}</dd><dt>proficient slope per SD</dt><dd>${S.prof.toFixed(3)}</dd><dt>limited-English slope per SD</dt><dd>${S.lim.toFixed(3)}</dd><dt>interaction</dt><dd>${(S.lim - S.prof).toFixed(3)}</dd></dl>`);
  }
  sel.on("change", draw); sl.on("input", draw); draw(); window.addEventListener("resize", draw);
}

// ---------- 3. cohorts ----------
function cohorts() {
  const co = R.results.cohort, mx = R.results.cohort_max_ysm, keys = ["pre1990", "1990s", "2000s", "2010s", "2020s"];
  const names = { pre1990: "Before 1990", "1990s": "1990s", "2000s": "2000s", "2010s": "2010s", "2020s": "2020s" };
  const cols = { pre1990: C.grey2, "1990s": C.grey, "2000s": C.tealLight, "2010s": C.teal, "2020s": C.tealDark };
  const on = new Set(keys);
  const tg = d3.select("#cohort-toggles");
  keys.forEach(k => tg.insert("button", "label").attr("class", "on").style("border-color", cols[k]).text(names[k]).on("click", function () { on.has(k) ? on.delete(k) : on.add(k); d3.select(this).classed("on", on.has(k)); draw(); }));
  const sl = d3.select("#co-t");
  const f = (k, t) => (Math.exp(co[k] + co.ysm * t + co.ysm2 * t * t) - 1) * 100;
  function draw() {
    const t0 = +sl.node().value; d3.select("#co-tv").text(t0);
    const { svg, w, h } = box("#s-cohort"); const m = { t: 30, r: 110, b: 40, l: 60 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const x = d3.scaleLinear().domain([0, 30]).range([0, cw]), y = d3.scaleLinear().domain([-26, 4]).range([ch, 0]);
    axes(g, x, y, cw, ch, { yf: d => d + "%" });
    g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", C.axis);
    g.append("text").attr("x", cw / 2).attr("y", ch + 34).attr("text-anchor", "middle").text("years since migration");
    g.append("line").attr("x1", x(t0)).attr("x2", x(t0)).attr("y1", 0).attr("y2", ch).attr("stroke", C.ink).attr("stroke-dasharray", "3 3");
    const rows = [], labels = [];
    keys.filter(k => on.has(k)).forEach(k => {
      const pts = d3.range(0, Math.min(30, mx[k]) + 1).map(t => ({ t, v: f(k, t) }));
      g.append("path").datum(pts).attr("d", d3.line().x(d => x(d.t)).y(d => y(d.v)).curve(d3.curveMonotoneX)).attr("fill", "none").attr("stroke", cols[k]).attr("stroke-width", 2.4);
      const e = pts[pts.length - 1]; labels.push({ x: x(e.t) + 8, y: y(e.v) + 4, t: names[k] });
      if (t0 <= mx[k]) { g.append("circle").attr("cx", x(t0)).attr("cy", y(f(k, t0))).attr("r", 5).attr("fill", cols[k]).attr("stroke", "#fff"); rows.push(`<dt>${names[k]}</dt><dd>${f(k, t0).toFixed(1)}%</dd>`); }
      else rows.push(`<dt>${names[k]}</dt><dd style="color:var(--muted)">not observed</dd>`);
    });
    labels.sort((a, b) => a.y - b.y); for (let i = 1; i < labels.length; i++) if (labels[i].y - labels[i - 1].y < 13) labels[i].y = labels[i - 1].y + 13;
    labels.forEach(l => g.append("text").attr("class", "label").attr("x", l.x).attr("y", l.y).text(l.t));
    d3.select("#d-cohort").html(`<h3>Gap to natives at ${t0} years</h3><dl>${rows.join("")}</dl><p style="margin:10px 0 0;font-size:12.5px;color:var(--muted)">Entry gaps: pre-2000 cohorts 21 to 22 percent, post-2000 cohorts 17 to 18 percent. Contrast 5.1 log points, SE 2.2, p = 0.02. Catch-up about 1.3 percent per year.</p>`);
  }
  sl.on("input", draw); draw(); window.addEventListener("resize", draw);
}

// ---------- 4. cells ----------
function cellsPanel() {
  const cells = R.results.cells.map(d => ({ ...d, suburb_prop: +d.suburb_prop, median_hourly: +d.median_hourly, limited_eng_prop: +d.limited_eng_prop, rent_burden: +d.rent_burden, n: +d.n_unweighted }));
  const regions = ["Asia", "Latin America", "Europe", "Africa"], cohorts = ["pre1990", "1990s", "2000s", "2010s"], windows = ["2012-2016", "2017-2021", "2022-2024"];
  const cn = { pre1990: "before 1990", "1990s": "1990s", "2000s": "2000s", "2010s": "2010s" };
  const on = new Set(regions), rb = d3.select("#cell-regions");
  regions.forEach(r => rb.append("button").attr("class", "on").style("border-color", C.region[r]).text(r).on("click", function () { on.has(r) ? on.delete(r) : on.add(r); d3.select(this).classed("on", on.has(r)); draw(); }));
  const sel = d3.select("#cell-metric");
  const fmts = { median_hourly: d => "$" + d.toFixed(0), suburb_prop: d => d.toFixed(0) + "%", limited_eng_prop: d => d.toFixed(0) + "%", rent_burden: d => d.toFixed(0) + "%" };
  const interp = { median_hourly: d3.interpolate("#f6efd9", "#7a5a10"), suburb_prop: d3.interpolate("#dff4f4", "#0a4f55"), limited_eng_prop: d3.interpolate("#fbe4cf", "#9a4d00"), rent_burden: d3.interpolate("#eeeeee", "#3d3d3d") };
  function draw() {
    const key = sel.node().value, fmt = fmts[key];
    const rows = []; regions.filter(r => on.has(r)).forEach(r => cohorts.forEach(c => rows.push(`${r} · ${cn[c]}`)));
    const rk = d => `${d.region} · ${cn[d.cohort]}`, data = cells.filter(d => on.has(d.region));
    const { svg, w, h } = box("#s-cells"); const m = { t: 40, r: 80, b: 10, l: 210 }, cw = w - m.l - m.r;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const y = d3.scaleBand().domain(rows).range([0, Math.min(h - m.t - m.b, rows.length * 26)]).padding(0.1), x = d3.scaleBand().domain(windows).range([0, Math.min(cw, 420)]).padding(0.08);
    windows.forEach(wn => g.append("text").attr("class", "label").attr("x", x(wn) + x.bandwidth() / 2).attr("y", -10).attr("text-anchor", "middle").text(wn.replace("-", "–")));
    rows.forEach(r => g.append("text").attr("x", -12).attr("y", y(r) + y.bandwidth() / 2 + 4).attr("text-anchor", "end").attr("fill", C.region[r.split(" · ")[0]]).style("cursor", "pointer").text(r)
      .on("click", () => show(r)));
    const ext = d3.extent(data, d => d[key]), sc = d3.scaleSequential(interp[key]).domain(ext);
    const cell = g.selectAll(".hc").data(data).join("g").attr("transform", d => `translate(${x(d.window)},${y(rk(d))})`).style("cursor", "pointer");
    cell.append("rect").attr("width", x.bandwidth()).attr("height", y.bandwidth()).attr("rx", 2).attr("fill", d => sc(d[key])).attr("stroke", "#fff");
    cell.append("text").attr("x", x.bandwidth() / 2).attr("y", y.bandwidth() / 2 + 4).attr("text-anchor", "middle").attr("class", "label").attr("fill", d => (d[key] - ext[0]) / (ext[1] - ext[0]) > 0.6 ? "#fff" : C.ink).text(d => fmt(d[key]));
    hover(cell, d => `<b>${rk(d)}, ${d.window.replace("-", "–")}</b><br>${fmt$(d.median_hourly)} median wage · ${d.suburb_prop.toFixed(0)}% suburban · ${d.limited_eng_prop.toFixed(0)}% limited English<br>n = ${fmtN(d.n)}`);
    cell.on("click", (ev, d) => show(rk(d)));
    function show(r) {
      const v = cells.filter(d => rk(d) === r).sort((a, b) => a.window.localeCompare(b.window));
      d3.select("#d-cells").html(`<h3>${r}</h3>` + v.map(d => `<div style="margin:6px 0 10px"><div style="font-family:var(--mono);font-size:12px;color:var(--muted)">${d.window.replace("-", "–")} · n = ${fmtN(d.n)}</div><dl><dt>median wage</dt><dd>${fmt$(d.median_hourly)}</dd><dt>outside the city</dt><dd>${d.suburb_prop.toFixed(1)}%</dd><dt>limited English</dt><dd>${d.limited_eng_prop.toFixed(1)}%</dd><dt>rent burden</dt><dd>${d.rent_burden.toFixed(1)}%</dd></dl></div>`).join(""));
    }
  }
  sel.on("change", draw); draw(); window.addEventListener("resize", draw);
}

// ---------- 5. coefficient table ----------
function coefTable() {
  const CG = R.results.coef_grid, cities = ["Philadelphia", "New York"];
  const rows = CG.rows.map((r, i) => ({ r, phl: CG.Philadelphia[i], nyc: CG["New York"][i] }));
  const t = d3.select("#coef-table").append("table").attr("class", "data");
  t.append("thead").append("tr").selectAll("th").data(["Coefficient", "Philadelphia", "New York", "Difference (SE)"]).join("th").text(d => d);
  const f = v => `${v[0] > 0 ? "+" : ""}${v[0].toFixed(3)} (${v[1].toFixed(3)})`;
  t.append("tbody").selectAll("tr").data(rows).join("tr").html(d => {
    const diff = d.phl[0] - d.nyc[0], se = Math.sqrt(d.phl[1] ** 2 + d.nyc[1] ** 2);
    return `<td>${d.r}</td><td>${f(d.phl)}</td><td>${f(d.nyc)}</td><td>${diff > 0 ? "+" : ""}${diff.toFixed(3)} (${se.toFixed(3)}), ${Math.abs(diff / se).toFixed(1)} SE</td>`;
  });
  // every interaction term
  let ichart;
  function drawI() {
    const { svg, w, h } = box("#s-coef");
    ichart = CH.interactions(svg, CH.INTERACTIONS(R.interactionsRegion), { width: w, height: h });
    ichart.bonferroni(d3.select("#inter-bonf").node().checked); ichart.layout(d3.select("#inter-sort").node().value);
  }
  d3.select("#inter-sort").on("change", () => ichart.layout(d3.select("#inter-sort").node().value));
  d3.select("#inter-bonf").on("change", () => ichart.bonferroni(d3.select("#inter-bonf").node().checked));
  drawI(); window.addEventListener("resize", drawI);
  d3.select("#coef-csv").on("click", () => {
    const csv = "coefficient,philadelphia,philadelphia_se,new_york,new_york_se\n" + rows.map(d => `"${d.r}",${d.phl[0]},${d.phl[1]},${d.nyc[0]},${d.nyc[1]}`).join("\n");
    const a = document.createElement("a"); a.href = URL.createObjectURL(new Blob([csv], { type: "text/csv" })); a.download = "coefficients_philadelphia_new_york.csv"; a.click();
  });
}

// ---------- 6. moves ----------
function moves() {
  const T4 = R.tables.find(t => t.number === "Table 4"), names = ["Moved within the city", "Moved within the suburbs", "City to suburb", "Suburb to city"];
  const parse = c => { const m = c.match(/([\d.]+) \(([\d.]+) to ([\d.]+)\)/); return m ? { rrr: +m[1], lo: +m[2], hi: +m[3] } : null; };
  const sel = d3.select("#mv-var");
  function draw() {
    const k = +sel.node().value, lab = T4.header[k], rr = T4.rows.slice(0, 4).map((r, i) => ({ move: names[i], ...parse(r[k]) }));
    const { svg, w, h } = box("#s-moves"); const m = { t: 30, r: 150, b: 40, l: 190 }, cw = w - m.l - m.r, ch = Math.min(420, h - m.t - m.b);
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const y = d3.scaleBand().domain(names).range([0, ch]).padding(0.5), x = d3.scaleLog().domain([0.3, 3]).range([0, cw]);
    g.append("g").attr("class", "grid").call(d3.axisLeft(y).tickSize(-cw).tickFormat(""));
    g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).tickValues([0.3, 0.5, 0.7, 1, 1.4, 2, 3]).tickFormat(d3.format(".1f")).tickSizeOuter(0));
    g.append("line").attr("x1", x(1)).attr("x2", x(1)).attr("y1", 0).attr("y2", ch).attr("stroke", C.ink);
    const r = g.selectAll("g.r").data(rr).join("g").attr("class", "r").attr("transform", d => `translate(0,${y(d.move) + y.bandwidth() / 2})`);
    r.append("text").attr("x", -12).attr("dy", 4).attr("text-anchor", "end").attr("class", "label").text(d => d.move);
    r.append("line").attr("x1", d => x(d.lo)).attr("x2", d => x(d.hi)).attr("stroke", d => d.lo > 1 || d.hi < 1 ? C.orange : C.grey).attr("stroke-width", 3).attr("stroke-linecap", "round");
    r.append("circle").attr("cx", d => x(d.rrr)).attr("r", 6).attr("fill", d => d.lo > 1 || d.hi < 1 ? C.orange : C.grey).attr("stroke", "#fff");
    r.append("text").attr("class", "label").attr("x", d => x(d.hi) + 10).attr("dy", 4).text(d => `${d.rrr.toFixed(2)} (${d.lo.toFixed(2)} to ${d.hi.toFixed(2)})`);
    hover(r, d => `<b>${d.move}</b><br>rrr ${d.rrr.toFixed(3)} per unit of ${lab.toLowerCase()}<br>95% interval ${d.lo.toFixed(2)} to ${d.hi.toFixed(2)}`);
    g.append("text").attr("x", cw / 2).attr("y", ch + 36).attr("text-anchor", "middle").text(`relative risk ratio per unit of ${lab.toLowerCase()}, log scale. Orange when the interval excludes one`);
  }
  sel.on("change", draw); draw(); window.addEventListener("resize", draw);
}

// ---------- 7. population ----------
function pop() {
  const tr = R.trend, or = R.origins;
  const { svg, w, h } = box("#s-pop"); const m = { t: 30, r: 20, b: 40, l: 70 }, lw = w * 0.55 - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const x = d3.scaleLinear().domain([2013, 2024]).range([0, lw]), y = d3.scaleLinear().domain([180000, 245000]).range([ch, 0]);
  axes(g, x, y, lw, ch, { xf: d3.format("d"), yf: d => (d / 1000) + "k" });
  g.append("text").attr("class", "title").attr("y", -12).text("Foreign-born residents");
  g.append("path").datum(tr).attr("d", d3.line().x(d => x(d.year)).y(d => y(d.foreign_born)).curve(d3.curveMonotoneX)).attr("fill", "none").attr("stroke", C.teal).attr("stroke-width", 2.6);
  const pts = g.selectAll("circle").data(tr).join("circle").attr("cx", d => x(d.year)).attr("cy", d => y(d.foreign_born)).attr("r", 4.5).attr("fill", C.teal).attr("stroke", "#fff");
  hover(pts, d => `<b>${d.year}</b><br>${fmtN(d.foreign_born)} foreign-born · ${d.pct_foreign_born}% of residents`);
  const ox = w * 0.55 + 200, ow = w - ox - 60;
  const g2 = svg.append("g").attr("transform", `translate(${ox},${m.t})`);
  const yb = d3.scaleBand().domain(or.map(d => d.country)).range([0, ch]).padding(0.25), xb = d3.scaleLinear().domain([0, 15]).range([0, ow]);
  g2.append("text").attr("class", "title").attr("y", -12).attr("x", -190).text("Country of birth, percent of the foreign-born");
  const b = g2.selectAll("g").data(or).join("g").attr("transform", d => `translate(0,${yb(d.country)})`);
  b.append("text").attr("x", -8).attr("y", yb.bandwidth() / 2 + 4).attr("text-anchor", "end").attr("class", "label").text(d => d.country);
  b.append("rect").attr("height", yb.bandwidth()).attr("width", d => xb(d.pct)).attr("fill", C.gold);
  b.append("text").attr("x", d => xb(d.pct) + 6).attr("y", yb.bandwidth() / 2 + 4).text(d => d.pct + "%");
  const last = tr[tr.length - 1], first = tr[0];
  d3.select("#d-pop").html(`<h3>${last.year}</h3><div class="big">${fmtN(last.foreign_born)}</div><dl><dt>percent of residents</dt><dd>${last.pct_foreign_born}%</dd><dt>in ${first.year}</dt><dd>${fmtN(first.foreign_born)} (${first.pct_foreign_born}%)</dd><dt>change</dt><dd>+${fmtN(last.foreign_born - first.foreign_born)}</dd><dt>analysis sample, all years</dt><dd>240,089</dd><dt>of which foreign-born</dt><dd>29,299</dd><dt>2022–2024 foreign-born</dt><dd>7,355</dd></dl>`);
}

// ---------- 8. wages by English ----------
function wageEng() {
  const t1 = R.results.t1, T1 = R.tables.find(t => t.number === "Table 1");
  const { svg, w, h } = box("#s-wage"); const m = { t: 30, r: 30, b: 40, l: 70 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const x = d3.scaleBand().domain(t1.map(d => d.eng)).range([0, cw]).padding(0.45), y = d3.scaleLinear().domain([0, 42]).range([ch, 0]);
  axes(g, x, y, cw, ch, { yf: d => "$" + d });
  g.append("text").attr("x", cw / 2).attr("y", ch + 34).attr("text-anchor", "middle").text("speaks English");
  const col = d3.scaleOrdinal().domain(t1.map(d => d.eng)).range([C.orange, C.gold, C.tealLight, C.teal]);
  const b = g.selectAll("g.b").data(t1).join("g").attr("class", "b");
  b.append("rect").attr("x", d => x(d.eng)).attr("y", d => y(d.median)).attr("width", x.bandwidth()).attr("height", d => ch - y(d.median)).attr("fill", d => col(d.eng)).attr("rx", 3);
  b.append("line").attr("x1", d => x(d.eng) + x.bandwidth() / 2).attr("x2", d => x(d.eng) + x.bandwidth() / 2).attr("y1", d => y(d.lo)).attr("y2", d => y(d.hi)).attr("stroke", C.ink).attr("stroke-width", 2);
  b.append("text").attr("x", d => x(d.eng) + x.bandwidth() / 2).attr("y", d => y(d.hi) - 8).attr("text-anchor", "middle").attr("class", "label").text(d => fmt$(d.median));
  hover(b, d => `<b>${d.eng}</b><br>median ${d3.format("$.2f")(d.median)}<br>95% interval ${d3.format("$.2f")(d.lo)} to ${d3.format("$.2f")(d.hi)}`);
  const row = k => T1.rows.find(r => r[0] === k);
  d3.select("#d-wage").html(`<h3>Table 1, main specification</h3><div class="big">0.059</div><p style="margin:0 0 8px;color:var(--muted)">log points per step on the four-point English scale (SE 0.006)</p>
    <dl><dt>female</dt><dd>${row("Female")[3]}</dd><dt>graduate degree</dt><dd>${row("Graduate degree")[3]}</dd><dt>less than high school</dt><dd>${row("Less than high school")[3]}</dd><dt>years since migration</dt><dd>${row("Years since migration")[3]}</dd><dt>observations</dt><dd>${row("Observations")[3]}</dd><dt>R-squared</dt><dd>${row("R-squared")[3]}</dd></dl>
    <p style="margin:10px 0 0;font-size:12.5px;color:var(--muted)">Occupation, PUMA-by-window and year fixed effects. Standard errors clustered on PUMA-by-window.</p>`);
}

// ---------- 9. decomposition ----------
function gapPanel() {
  const O = R.results.oaxaca, specs = [["Without English", O.base], ["With English", O.with_english]];
  const { svg, w, h } = box("#s-gap"); const m = { t: 30, r: 30, b: 40, l: 70 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const x = d3.scaleBand().domain(specs.map(d => d[0])).range([0, cw]).padding(0.5), y = d3.scaleLinear().domain([0, 0.12]).range([ch, 0]);
  axes(g, x, y, cw, ch, { yf: d3.format(".2f") });
  g.append("text").attr("x", -50).attr("y", -12).attr("class", "title").text("log points of the native to foreign-born gap");
  specs.forEach(([n, v]) => {
    const parts = [["explained", v.explained, C.teal], ["unexplained", v.unexplained, C.orange]]; let acc = 0;
    parts.forEach(([k, val, c]) => {
      const r = g.append("rect").attr("x", x(n)).attr("y", y(acc + val)).attr("width", x.bandwidth()).attr("height", y(acc) - y(acc + val)).attr("fill", c).attr("stroke", "#fff");
      g.append("text").attr("x", x(n) + x.bandwidth() / 2).attr("y", y(acc + val / 2) + 4).attr("text-anchor", "middle").attr("fill", "#fff").text(`${k} ${val.toFixed(3)}`);
      hover(r, () => `<b>${n}</b><br>${k} ${val.toFixed(3)} of ${O.gap.toFixed(3)}`);
      acc += val;
    });
  });
  g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(O.gap)).attr("y2", y(O.gap)).attr("stroke", C.ink).attr("stroke-dasharray", "3 3");
  g.append("text").attr("x", cw).attr("y", y(O.gap) - 6).attr("text-anchor", "end").attr("class", "label").text(`total gap ${O.gap.toFixed(3)}`);
  d3.select("#d-gap").html(`<h3>Oaxaca–Blinder, natives as reference</h3><div class="big">${O.gap.toFixed(3)}</div><p style="margin:0 0 8px;color:var(--muted)">log point gap, 2022–2024</p>
    <dl><dt>explained, without English</dt><dd>${O.base.explained.toFixed(3)}</dd><dt>unexplained, without English</dt><dd>${O.base.unexplained.toFixed(3)}</dd><dt>explained, with English</dt><dd>${O.with_english.explained.toFixed(3)}</dd><dt>unexplained, with English</dt><dd>${O.with_english.unexplained.toFixed(3)}</dd></dl>
    <p style="margin:10px 0 0;font-size:12.5px;color:var(--muted)">Adding the English score moves ${(O.base.unexplained - O.with_english.unexplained).toFixed(3)} log points from the unexplained to the explained part.</p>`);
}

// ---------- occupations ----------
function occPanel() {
  if (!R.occupations) return;
  let chart;
  function draw() { const { svg, w, h } = box("#s-occ"); chart = CH.occMatrix(svg, R.occupations, { width: w, height: h }); chart.sortBy(d3.select("#occ-sort").node().value); }
  d3.select("#occ-sort").on("change", () => chart.sortBy(d3.select("#occ-sort").node().value));
  const byG = g => R.occupations.filter(r => r.group === g);
  const top = g => byG(g).slice().sort((a, b) => b.pct - a.pct).slice(0, 3).map(r => `${r.occ_group} ${(+r.pct).toFixed(0)}%`).join(" · ");
  d3.select("#d-occ").html(`<h3>Three largest occupation groups</h3><dl><dt>Limited English</dt><dd style="text-align:left">${top("Limited English")}</dd><dt>Proficient immigrants</dt><dd style="text-align:left">${top("Proficient immigrants")}</dd><dt>Native-born</dt><dd style="text-align:left">${top("Native-born")}</dd></dl>
    <p style="margin:10px 0 0;font-size:12.5px;color:var(--muted)">Descriptive weighted percentages and medians for the 2022–2024 analysis sample, wage and salary workers aged 25 to 64. Occupation groups follow the ACS OCCP code ranges.</p>`);
  draw(); window.addEventListener("resize", draw);
}

// ---------- move flow ----------
function flowPanel() {
  let chart;
  function draw() { const { svg, w, h } = box("#s-flow"); chart = CH.moveFlow(svg, R.moveRates, { width: w, height: h, window: d3.select("#flow-window").node().value, fb: d3.select("#flow-group").node().value === "fb", exclude: d3.select("#flow-movers").node().checked }); }
  d3.selectAll("#flow-window, #flow-group, #flow-movers").on("change", draw); window.addEventListener("resize", draw);
  draw();
}

// ---------- full-screen flow globe ----------
function flowMapPanel() {
  if (!R.pumas || !R.origins2) return;
  let chart;
  function draw() { const { svg, w, h } = box("#s-flowmap"); chart = CH.flowMap(svg, R.pumas, { width: w, height: h, origins: R.origins2, flows: [], countries: R.countries }); chart.world(); }
  draw(); window.addEventListener("resize", draw);
}

// ---------- counties ----------
function countyPanel() {
  if (!R.counties) return;
  function draw() { const { svg, w, h } = box("#s-county"); CH.countyRose(svg, R.counties, { width: w, height: h }); }
  draw(); window.addEventListener("resize", draw);
}

// ---------- 10. every table ----------
function tablesPanel() {
  const T = R.tables, bt = d3.select("#tab-buttons"), body = d3.select("#tab-body");
  function show(i) {
    bt.selectAll("button").classed("on", (d, j) => j === i);
    const t = T[i];
    const csv = [t.header].concat(t.rows).map(r => r.map(c => `"${c}"`).join(",")).join("\n");
    body.html(`<h3>${t.number}. ${t.caption}</h3>`);
    const tb = body.append("table").attr("class", "data");
    tb.append("thead").append("tr").selectAll("th").data(t.header).join("th").text(d => d);
    tb.append("tbody").selectAll("tr").data(t.rows).join("tr").classed("sub", d => d[0].startsWith("   ")).selectAll("td").data(d => d).join("td").classed("empty", d => d === "").text(d => d === "" ? "·" : d);
    body.append("p").attr("class", "note").text(t.note);
    body.append("button").style("margin-top", "10px").attr("class", "csv").text("Download CSV").on("click", () => {
      const a = document.createElement("a"); a.href = URL.createObjectURL(new Blob([csv], { type: "text/csv" })); a.download = t.number.toLowerCase().replace(" ", "_") + ".csv"; a.click();
    });
  }
  bt.selectAll("button").data(T).join("button").text(d => d.number).on("click", (ev, d) => show(T.indexOf(d)));
  show(0);
}

async function boot() {
  R.results = await d3.json("data/results.json");
  R.tables = await d3.json("data/tables.json");
  R.trend = (await d3.csv("data/trend.csv")).map(d => ({ year: +d.year, foreign_born: +d.foreign_born, pct_foreign_born: +d.pct_foreign_born }));
  R.origins = (await d3.csv("data/origins.csv")).map(d => ({ ...d, pct: +d.pct }));
  try { R.pumas = await d3.json("data/pumas.geojson"); } catch (e) { R.pumas = null; }
  try { R.mobility = await d3.json("data/mobility.json"); } catch (e) { R.mobility = null; }
  try { R.origins2 = await d3.csv("data/origin_puma.csv"); } catch (e) { R.origins2 = null; }
  try { const wd = await d3.json("data/countries-110m.json"); R.countries = topojson.feature(wd, wd.objects.countries); } catch (e) { R.countries = null; }
  try { R.occupations = await d3.csv("data/occupations.csv"); } catch (e) { R.occupations = null; }
  try { R.counties = await d3.csv("data/county_profiles.csv"); } catch (e) { R.counties = null; }
  try { const mr = await d3.csv("data/move_rates.csv"); R.moveRates = mr.map(d => ({ ...d, fb: d.fb === "TRUE" || d.fb === "true", n: +d.n, pct: +d.pct })); }
  catch (e) { R.moveRates = R.mobility ? R.mobility.move_rates.map(d => ({ ...d, fb: true })) : []; }
  try { const ir = await d3.csv("data/interactions_region.csv"); R.interactionsRegion = ir.filter(d => d.Estimate).map(d => ({ region: d.sample, est: d.Estimate, se: d["Std. Error"] })); } catch (e) { R.interactionsRegion = []; }
  flowMapPanel(); areas(); calc(); cohorts(); occPanel(); cellsPanel(); coefTable(); flowPanel(); moves(); tablesPanel(); window.dispatchEvent(new Event("resize"));
}
boot();

})();
