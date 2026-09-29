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
    if (!p) { d3.select("#d-area").html(`<h3>Hover or click an area</h3><p style="color:var(--muted)">The county names are drawn on the map; each PUMA holds about 100,000 people.</p>`); return; }
    d3.select("#d-area").html(`<h3>${p.name.replace(" PUMA", "")}</h3><div class="big">${(+p.puma_fb_prop).toFixed(1)}%</div><p style="margin:0 0 8px;color:var(--muted)">foreign-born</p>
      <dl><dt>limited English among foreign-born</dt><dd>${(+p.fb_limited_prop).toFixed(1)}%</dd><dt>foreign-born median wage</dt><dd>${fmt$(+p.fb_median_wage)}</dd><dt>location</dt><dd>${p.county ? p.county + (p.county.includes("&") ? " counties" : " County") : (p.city ? "Philadelphia County" : "suburban county")}</dd></dl>`);
  }
  let chart;
  function draw() {
    const { svg, w, h } = box("#s-area");
    if (!G) { svg.append("text").attr("x", 20).attr("y", 40).attr("class", "title").text("Upload data/pumas.geojson for the map"); return; }
    chart = CH.linkedMap(svg, G, { width: w, height: h, mode: "map", counties: R.countyShapes, mobility: R.mobility ? R.mobility.flows : null, onSelect: detail });
    chart.color(d3.select("#area-var").node().value); chart.trend(true);
    chart.showFlows(d3.select("#area-flows").node().value !== "none", d3.select("#area-flows").node().value);
  }
  d3.select("#area-var").on("change", () => chart && chart.color(d3.select("#area-var").node().value));
  d3.select("#area-flows").on("change", () => { const v = d3.select("#area-flows").node().value; chart && chart.showFlows(v !== "none", v); });
  detail(null); draw(); window.addEventListener("resize", draw);
}

// ---------- 2. differential calculator ----------
function calc() {
  const CG = R.results.coef_grid, E = R.results.enclave;
  const v = (city, i) => CG[city][i][0];
  // coef_grid rows: 0 limited, 1 density, 2 interaction, 4 limited within occ, 5 density within occ, 6 interaction within occ (Tables 2 and 3)
  const panels = [
    { title: "Philadelphia, all occupations", note: `limited-English coefficient ${d3.format("+.3f")(E.A.limited)} · interaction ${d3.format("+.3f")(E.A.inter)}`, gap: E.A.limited, prof: E.A.density, lim: E.A.density + E.A.inter },
    { title: "Philadelphia, within occupation", note: `limited-English coefficient ${d3.format("+.3f")(E.B.limited)} · interaction ${d3.format("+.3f")(E.B.inter)}`, gap: E.B.limited, prof: E.B.density, lim: E.B.density + E.B.inter },
    { title: "New York, all occupations", note: `limited-English coefficient ${d3.format("+.3f")(v("New York", 0))} · interaction ${d3.format("+.3f")(v("New York", 2))}`, gap: v("New York", 0), prof: v("New York", 1), lim: v("New York", 1) + v("New York", 2) },
    { title: "New York, within occupation", note: `limited-English coefficient ${d3.format("+.3f")(v("New York", 4))} · interaction ${d3.format("+.3f")(v("New York", 6))}`, gap: v("New York", 4), prof: v("New York", 5), lim: v("New York", 5) + v("New York", 6) }
  ];
  const sl = d3.select("#calc-d");
  let chart;
  function detail() {
    const d0 = +sl.node().value; d3.select("#calc-dv").text(`${d0 >= 0 ? "+" : ""}${d0.toFixed(1)} SD`);
    const P = chart.panels;
    d3.select("#d-calc").html(`<h3>Limited-English gap at ${d0 >= 0 ? "+" : ""}${d0.toFixed(1)} SD</h3><div>Philadelphia <b>${P[0].gap.toFixed(0)}%</b>, within occupation <b>${P[1].gap.toFixed(0)}%</b></div><div>New York <b>${P[2].gap.toFixed(0)}%</b>, within occupation <b>${P[3].gap.toFixed(0)}%</b></div>`);
  }
  function draw() { const { svg, w, h } = box("#s-calc"); chart = CH.occPanels(svg, { width: w, height: h, panels, d: +sl.node().value }); window.occPanelsChart = chart; detail(); }
  sl.on("input", () => { chart.set(+sl.node().value); detail(); }); draw(); window.addEventListener("resize", draw);
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
  // country of birth cells (data/cells_country.csv): window x country, at least 50 respondents in the window
  const CR = { "Africa": "Africa", "Egypt": "Africa", "Ghana": "Africa", "Kenya": "Africa", "Liberia": "Africa", "Morocco": "Africa", "Nigeria": "Africa", "Sierra Leone": "Africa",
    "Bangladesh": "Asia", "Cambodia": "Asia", "China": "Asia", "Hong Kong": "Asia", "India": "Asia", "Iran": "Asia", "Israel": "Asia", "Korea": "Asia", "Pakistan": "Asia", "Philippines": "Asia", "Taiwan": "Asia", "Thailand": "Asia", "Turkey": "Asia", "Vietnam": "Asia",
    "Albania": "Europe", "Belarus": "Europe", "England": "Europe", "France": "Europe", "Germany": "Europe", "Greece": "Europe", "Ireland": "Europe", "Italy": "Europe", "Poland": "Europe", "Portugal": "Europe", "Romania": "Europe", "Russia": "Europe", "USSR": "Europe", "Ukraine": "Europe", "United Kingdom, Not Specified": "Europe",
    "Brazil": "Latin America", "Colombia": "Latin America", "Cuba": "Latin America", "Dominican Republic": "Latin America", "Ecuador": "Latin America", "El Salvador": "Latin America", "Guatemala": "Latin America", "Guyana": "Latin America", "Haiti": "Latin America", "Honduras": "Latin America", "Jamaica": "Latin America", "Mexico": "Latin America", "Peru": "Latin America", "Trinidad and Tobago": "Latin America", "Venezuela": "Latin America",
    "Canada": "Northern America" };
  const CN = { "United Kingdom, Not Specified": "UK (unspecified)", "Africa": "Africa (unspec.)", "USSR": "USSR (unspec.)", "Dominican Republic": "Dominican Rep.", "Trinidad and Tobago": "Trinidad & Tobago", "Sierra Leone": "Sierra Leone" };
  const cells = (R.cellsCountry || []).map(d => ({ country: d.country, label: CN[d.country] || d.country, region: CR[d.country] || "Other", window: d.window, n: +d.n, n_wt: +d.n_wt,
    median_hourly: +d.median_hourly, limited_eng_prop: +d.limited_pct, suburb_prop: (1 - +d.city_prop) * 100 }));
  if (!cells.length) return;
  const regions = ["Asia", "Latin America", "Europe", "Africa", "Northern America"], windows = ["2012-2016", "2017-2021", "2022-2024"];
  const col2 = r => C.region[r] || "#6b6b6b";
  const on = new Set(regions), rb = d3.select("#cell-regions");
  rb.selectAll("button").data(regions).join("button").attr("class", "on").style("border-color", d => col2(d)).text(d => d).on("click", function (ev, r) { on.has(r) ? on.delete(r) : on.add(r); d3.select(this).classed("on", on.has(r)); draw(); });
  const sel = d3.select("#cell-metric");
  const fmts = { median_hourly: d => "$" + d.toFixed(0), suburb_prop: d => d.toFixed(0) + "%", limited_eng_prop: d => d.toFixed(0) + "%" };
  const interp = { median_hourly: d3.interpolate("#f6efd9", "#7a5a10"), suburb_prop: d3.interpolate("#dff4f4", "#0a4f55"), limited_eng_prop: d3.interpolate("#fbe4cf", "#9a4d00") };
  const size = d3.rollup(cells, v => d3.sum(v, d => d.n_wt), d => d.label);
  function draw() {
    const key = sel.node().value, fmt = fmts[key];
    const bands = regions.filter(r => on.has(r)).map(r => ({ region: r, countries: [...new Set(cells.filter(d => d.region === r).map(d => d.label))].sort((a, b) => size.get(b) - size.get(a)) }));
    const { svg, w, h } = box("#s-cells"); const m = { t: 6, r: 10, b: 30, l: 0 }, cw = w - m.l - m.r, H = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const labelW = 104, headH = 36, gapX = 30, gapY = 22;
    // vertical heatmaps: countries as rows, the three windows as columns, regions packed into as few columns as fit
    let cs = 40, cols = [];
    const bh = (b, c) => headH + b.countries.length * (c + 2) + gapY;
    for (; cs >= 12; cs -= 1) {
      // best fit: tallest bands first, each into the column with the most room that still fits
      cols = []; const heights = [];
      bands.slice().sort((a, b) => b.countries.length - a.countries.length).forEach(b => {
        let best = -1; heights.forEach((hh, i) => { if (hh + bh(b, cs) <= H && (best < 0 || hh < heights[best])) best = i; });
        if (best < 0) { cols.push([b]); heights.push(bh(b, cs)); } else { cols[best].push(b); heights[best] += bh(b, cs); }
      });
      const colW = labelW + 3 * (cs + 2);
      if (cols.length * colW + (cols.length - 1) * gapX <= cw && bands.every(b => bh(b, cs) - gapY <= H)) break;
    }
    // keep the paper's region order within each column
    cols.forEach(c => c.sort((a, b) => regions.indexOf(a.region) - regions.indexOf(b.region)));
    const colW = labelW + 3 * (cs + 2);
    const ext = d3.extent(cells.filter(d => on.has(d.region)), d => d[key]), sc = d3.scaleSequential(interp[key]).domain(ext);
    const fs = cs >= 30 ? 11 : cs >= 22 ? 9.5 : 0;
    cols.forEach((col, ci) => {
      let yy = 0;
      col.forEach(b => {
        const bg = g.append("g").attr("transform", `translate(${ci * (colW + gapX) + labelW},${yy})`);
        bg.append("text").attr("class", "label").attr("x", -labelW).attr("y", 12).style("font-weight", 700).attr("fill", col2(b.region)).text(b.region + "-born");
        const x = d3.scaleBand().domain(windows).range([0, 3 * (cs + 2)]).paddingInner(2 / (cs + 2)), y = d3.scaleBand().domain(b.countries).range([headH, headH + b.countries.length * (cs + 2)]).paddingInner(2 / (cs + 2));
        windows.forEach(wn => bg.append("text").attr("class", "note").attr("x", x(wn) + x.bandwidth() / 2).attr("y", headH - 6).attr("text-anchor", "middle").attr("fill", "#595959").style("font-size", cs >= 24 ? "10px" : "8.5px").text(cs >= 38 ? wn.replace("-", "–") : wn.slice(2, 4) + "–" + wn.slice(7)));
        const data = cells.filter(d => d.region === b.region);
        const cell = bg.selectAll("g.hc").data(data).join("g").attr("class", "hc").attr("transform", d => `translate(${x(d.window)},${y(d.label)})`).style("cursor", "pointer");
        cell.append("rect").attr("width", x.bandwidth()).attr("height", y.bandwidth()).attr("rx", 3).attr("fill", d => sc(d[key])).attr("stroke", "#fff");
        if (fs) cell.append("text").attr("x", x.bandwidth() / 2).attr("y", y.bandwidth() / 2 + fs * 0.35).attr("text-anchor", "middle").attr("class", "label").style("font-size", fs + "px").attr("fill", d => (d[key] - ext[0]) / (ext[1] - ext[0]) > 0.6 ? "#fff" : C.ink).text(d => fmt(d[key]));
        hover(cell, d => `<b>${d.country}, ${d.window.replace("-", "–")}</b><br>${fmt$(d.median_hourly)} median hourly wage, 2024 dollars · ${d.limited_eng_prop.toFixed(0)}% limited English · ${d.suburb_prop.toFixed(0)}% outside the city<br>n = ${fmtN(d.n)} respondents`);
        cell.on("click", (ev, d) => show(d.label));
        bg.selectAll("text.cn").data(b.countries).join("text").attr("class", "note cn").attr("x", -8).attr("y", d => y(d) + y.bandwidth() / 2 + 3.5).attr("text-anchor", "end").attr("fill", C.ink).style("font-size", cs >= 24 ? "11px" : "10px").style("cursor", "pointer").text(d => d).on("click", (ev, d) => show(d));
        yy += headH + b.countries.length * (cs + 2) + gapY;
      });
    });
    // legend
    const lg = g.append("g").attr("transform", `translate(${cw - 120},${H + 6})`);
    const ls = d3.scaleLinear().domain(ext).range([0, 110]);
    d3.range(0, 111, 3).forEach(v => lg.append("rect").attr("x", v).attr("y", 0).attr("width", 3).attr("height", 8).attr("fill", sc(ls.invert(v))));
    lg.append("text").attr("class", "note").attr("x", 0).attr("y", 20).text(fmt(ext[0])); lg.append("text").attr("class", "note").attr("x", 110).attr("y", 20).attr("text-anchor", "end").text(fmt(ext[1]));
    function show(lab) {
      const v = cells.filter(d => d.label === lab).sort((a, b) => a.window.localeCompare(b.window));
      d3.select("#d-cells").html(`<h3>${v[0].country}</h3>` + v.map(d => `<div><span style="color:var(--muted)">${d.window.replace("-", "–")}, n = ${fmtN(d.n)}:</span> <b>${fmt$(d.median_hourly)}</b> median wage · <b>${d.limited_eng_prop.toFixed(0)}%</b> limited English · <b>${d.suburb_prop.toFixed(0)}%</b> outside the city</div>`).join(""));
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
  // chapter 10: Philadelphia against New York (Table 3 and Table 2)
  const g = CG, r = i => ({ phl: g.Philadelphia[i], nyc: g["New York"][i] });
  const blocks = [
    { title: "Limited-English differential, log points", domain: [-0.32, 0.02], fmt: d3.format("+.2f"), rows: [
      { label: "all occupations", ...r(0), note: "21% in Philadelphia, 20% in New York" },
      { label: "within occupation", ...r(4), note: "9% and 10%: the same split" }] },
    { title: "Wage change per SD of immigrant density, proficient workers", domain: [-0.13, 0.02], fmt: d3.format("+.2f"), rows: [
      { label: "all occupations", ...r(1), note: "1 SD: 5.4 points here, 12.7 in New York" },
      { label: "within occupation", ...r(5), note: "" }] },
    { title: "Does the differential change with density? The interaction", domain: [-0.1, 0.12], fmt: d3.format("+.2f"), rows: [
      { label: "generic density", ...r(2), note: "marginal or null in both, one SE apart" },
      { label: "generic density, within occupation", ...r(6), note: "" },
      { label: "co-ethnic density", ...r(3), note: "opposite signs, two SE apart" }] },
    { title: "The same interaction on own-language density, exploratory", domain: [-0.1, 0.12], fmt: d3.format("+.2f"), rows: [
      { label: "own-language density", ...r(7), note: "the two areas agree" },
      { label: "own-language, within occupation", ...r(8), note: "New York's are 5 and 4 SE from zero" }] }
  ];
  let cchart;
  function drawI() { const { svg, w, h } = box("#s-coef"); cchart = CH.cityCompare(svg, { width: w, height: h, blocks }); window.cityCompareChart = cchart; cchart.bonferroni(d3.select("#inter-bonf").node().checked); }
  d3.select("#inter-bonf").on("change", () => cchart.bonferroni(d3.select("#inter-bonf").node().checked));
  drawI(); window.addEventListener("resize", drawI);
  d3.select("#coef-csv").on("click", () => {
    const csv = "coefficient,philadelphia,philadelphia_se,new_york,new_york_se\n" + rows.map(d => `"${d.r}",${d.phl[0]},${d.phl[1]},${d.nyc[0]},${d.nyc[1]}`).join("\n");
    const a = document.createElement("a"); a.href = URL.createObjectURL(new Blob([csv], { type: "text/csv" })); a.download = "coefficients_philadelphia_new_york.csv"; a.click();
  });
}

// ---------- 6. moves ----------
function moves() {
  const T4 = R.tables.find(t => t.number === "Table 4"), names = ["Moved within the city", "Moved within the suburbs", "City to suburb", "Suburb to city"], types = ["within_city", "within_suburbs", "city_to_suburb", "suburb_to_city"];
  const parse = c => { const m = c.match(/([\d.]+) \(([\d.]+) to ([\d.]+)\)/); return m ? { rrr: +m[1], lo: +m[2], hi: +m[3] } : null; };
  const sel = d3.select("#mv-var");
  const TXT = { 1: ["How the chance of each type of move changes with pay", "a worker whose hourly wage is 2.7 times higher (one unit of log wage), against staying put"],
    2: ["How the chance of each type of move changes with English", "one step up the four-point English scale, against staying put"],
    3: ["How the chance of each type of move changes with a graduate degree", "a worker with a graduate degree against one without, against staying put"],
    4: ["How the chance of each type of move changes with marriage", "a married worker against an unmarried one, against staying put"] };
  let rows = [], rowSel = null;
  const colorFor = d => !d ? "#a1a1a1" : d.lo > 1 ? C.orange : d.hi < 1 ? C.teal : d.rrr > 1 ? "#f3c9a3" : "#9fdcdc";
  function draw() {
    const k = +sel.node().value, rr = T4.rows.slice(0, 4).map((r, i) => ({ move: names[i], type: types[i], ...parse(r[k]) }));
    rows = rr; window.flowLink.colorOf = t => colorFor(rr.find(d => d.type === t));
    const { svg, w, h } = box("#s-moves"); if (!svg.node().getBoundingClientRect().width) return;
    const m = { t: 56, r: 150, b: 44, l: 190 }, cw = w - m.l - m.r, ch = Math.min(220, h - m.t - m.b);
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    g.append("text").attr("class", "title").attr("x", -m.l + 10).attr("y", -38).text(TXT[k][0]);
    g.append("text").attr("class", "note").attr("x", -m.l + 10).attr("y", -22).attr("fill", "#7f7f7f").text("relative risk in a year for " + TXT[k][1]);
    g.append("text").attr("class", "note").attr("x", -m.l + 10).attr("y", -8).attr("fill", "#7f7f7f").text("foreign-born workers, three windows pooled · multinomial logit, Table 4 · hover a row to trace its ribbon");
    const y = d3.scaleBand().domain(names).range([0, ch]).padding(0.5), x = d3.scaleLog().domain([0.3, 3]).range([0, cw]);
    g.append("g").attr("class", "grid").call(d3.axisLeft(y).tickSize(-cw).tickFormat(""));
    g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).tickValues([0.3, 0.5, 0.7, 1, 1.4, 2, 3]).tickFormat(d => d === 1 ? "same" : d + "×").tickSizeOuter(0));
    g.append("line").attr("x1", x(1)).attr("x2", x(1)).attr("y1", 0).attr("y2", ch).attr("stroke", C.ink);
    const r = g.selectAll("g.r").data(rr).join("g").attr("class", "r").attr("transform", d => `translate(0,${y(d.move) + y.bandwidth() / 2})`).style("cursor", "pointer");
    rowSel = r;
    r.append("text").attr("x", -12).attr("dy", 4).attr("text-anchor", "end").attr("class", "label").text(d => d.move);
    r.append("line").attr("x1", d => x(d.lo)).attr("x2", d => x(d.hi)).attr("stroke", d => colorFor(d)).attr("stroke-width", 4).attr("stroke-linecap", "round");
    r.append("circle").attr("cx", d => x(d.rrr)).attr("r", 6).attr("fill", d => colorFor(d)).attr("stroke", "#fff");
    r.append("text").attr("class", "label").attr("x", d => x(d.hi) + 10).attr("dy", 4).text(d => `${d.rrr.toFixed(2)}× (${d.lo.toFixed(2)} to ${d.hi.toFixed(2)})`);
    hover(r, d => `<b>${d.move}</b><br>${d.rrr.toFixed(2)} times as likely as staying, for ${TXT[k][1].split(", against")[0]}<br>95% interval ${d.lo.toFixed(2)} to ${d.hi.toFixed(2)}${d.lo > 1 || d.hi < 1 ? "" : " · includes 1, so not distinguishable from no change"}`);
    r.on("mouseenter", (ev, d) => highlight(d.type)).on("mouseleave", () => highlight(null));
    g.append("text").attr("class", "note").attr("x", x(1) - 8).attr("y", ch + 32).attr("text-anchor", "end").attr("fill", "#7f7f7f").text("← less likely to make this move");
    g.append("text").attr("class", "note").attr("x", x(1) + 8).attr("y", ch + 32).attr("fill", "#7f7f7f").text("more likely →");
    g.append("text").attr("class", "note").attr("x", cw + 140).attr("y", ch + 32).attr("text-anchor", "end").attr("fill", "#7f7f7f").text("strong color: interval excludes 1");
    if (window.flowLink.redraw) window.flowLink.redraw();
  }
  function highlight(t) { if (rowSel) rowSel.transition().duration(200).style("opacity", d => !t || d.type === t ? 1 : 0.25); if (window.flowLink.highlight) window.flowLink.highlight(t); }
  window.flowLink.onHover = t => { if (rowSel) rowSel.style("opacity", d => !t || d.type === t ? 1 : 0.25); };
  window.flowLink.highlightAll = highlight;
  sel.on("change", draw); window.drawMoves = draw; draw(); window.addEventListener("resize", draw);
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
  const wins = ["2012-2016", "2017-2021", "2022-2024"];
  let panels = [], focused = null;
  window.flowLink = { colorOf: null, onHover: null };
  function draw() {
    const fb = d3.select("#flow-group").node().value === "fb", excl = d3.select("#flow-movers").node().checked;
    const { svg, w, h } = box("#s-flow");
    svg.append("text").attr("class", "title").attr("x", 0).attr("y", 16).text(`${fb ? "Foreign-born" : "Native-born"} adults 25 to 64, one year of moves in each window${excl ? ", movers only" : ""}`);
    svg.append("text").attr("class", "note").attr("x", 0).attr("y", 31).attr("fill", "#7f7f7f").text("percentages of all adults in the group · ribbon color from the model below: teal less likely, orange more likely, grey not in the model");
    const ph = (h - 40) / 3;
    panels = wins.map((wn, i) => { const g = svg.append("g").attr("transform", `translate(0,${40 + i * ph})`); const c = CH.moveFlow(g, R.moveRates, { width: w, height: ph, window: wn, fb, exclude: excl, compact: true, colorOf: t => window.flowLink.colorOf ? window.flowLink.colorOf(t) : "#a1a1a1", onHover: t => { if (window.flowLink.onHover) window.flowLink.onHover(t); highlight(t); } }); return { g, c, wn }; });
    focusWindow(focused);
  }
  function highlight(t) { panels.forEach(p => p.c.highlight(t)); }
  function focusWindow(wn) { focused = wn; panels.forEach(p => p.g.transition().duration(600).style("opacity", !wn || p.wn === wn ? 1 : 0.2)); }
  window.flowLink.highlight = highlight; window.flowLink.focusWindow = focusWindow; window.flowLink.redraw = draw;
  d3.selectAll("#flow-group, #flow-movers").on("change", draw); window.addEventListener("resize", draw);
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
  try { R.cellsCountry = await d3.csv("data/cells_country.csv"); } catch (e) { R.cellsCountry = null; }
  try { R.countyShapes = await d3.json("data/counties.geojson"); } catch (e) { R.countyShapes = null; }
  try { const mr = await d3.csv("data/move_rates.csv"); R.moveRates = mr.map(d => ({ ...d, fb: d.fb === "TRUE" || d.fb === "true", n: +d.n, pct: +d.pct })); }
  catch (e) { R.moveRates = R.mobility ? R.mobility.move_rates.map(d => ({ ...d, fb: true })) : []; }
  try { const ir = await d3.csv("data/interactions_region.csv"); R.interactionsRegion = ir.filter(d => d.Estimate).map(d => ({ region: d.sample, est: d.Estimate, se: d["Std. Error"] })); } catch (e) { R.interactionsRegion = []; }
  flowMapPanel(); areas(); calc(); cohorts(); occPanel(); cellsPanel(); coefTable(); flowPanel(); moves(); tablesPanel(); window.dispatchEvent(new Event("resize"));
}
boot();

})();
