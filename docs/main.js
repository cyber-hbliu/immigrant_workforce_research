/* Immigrant wages in metropolitan Philadelphia. Scrollytelling, D3 v7 + scrollama.
   Data (web/data/): trend.csv, origins.csv, land-110m.json, results.json */

const C = {
  ink: "#0d0d0d", ink2: "#595959", muted: "#7f7f7f", grid: "#ececec", axis: "#bababa",
  teal: "#00b0be", tealDark: "#0d7d87", tealPale: "#dff4f4", tealLight: "#8fd7d7",
  orange: "#ea801c", orangeLight: "#f0b077", gold: "#c99b38", grey: "#a1a1a1", grey2: "#d4d4d4", brown: "#eddca5",
  region: { "Asia": "#00b0be", "Latin America": "#ea801c", "Europe": "#c99b38", "Africa": "#a1a1a1" },
  english: ["#8fd7d7", "#00b0be", "#0d7d87", "#0a4f55"]
};
const PHL = [-75.1652, 39.9526];
const fmt$ = d3.format("$,.0f"), fmtPct = d => d3.format(".0f")(d) + "%", fmtN = d3.format(",");
const ease = d3.easeCubicInOut, T = 900;
const state = { data: {}, world: null, charts: {} };

// ---------- helpers ----------
function svgBox(id) {
  const svg = d3.select(id); svg.selectAll("*").remove();
  const { width, height } = svg.node().getBoundingClientRect();
  svg.attr("viewBox", `0 0 ${width} ${height}`);
  return { svg, w: width, h: height };
}
function caption() {}
const tip = d3.select("body").append("div").attr("class", "tip").style("opacity", 0);
function hover(sel, html) {
  sel.style("cursor", "default")
    .on("mousemove", (ev, d) => { tip.style("opacity", 1).html(html(d)).style("left", (ev.pageX + 14) + "px").style("top", (ev.pageY - 10) + "px"); })
    .on("mouseleave", () => tip.style("opacity", 0));
}
function drawPath(sel, dur = 1200, delay = 0) {
  sel.each(function () {
    const L = this.getTotalLength();
    d3.select(this).attr("stroke-dasharray", `${L} ${L}`).attr("stroke-dashoffset", L)
      .transition("draw").delay(delay).duration(dur).ease(ease).attr("stroke-dashoffset", 0);
  });
}
function axes(g, x, y, w, h, opts = {}) {
  g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(opts.yt || 5).tickSize(-w).tickFormat(""));
  g.append("g").attr("class", "axis").attr("transform", `translate(0,${h})`).call(d3.axisBottom(x).ticks(opts.xt || 6).tickFormat(opts.xf || null).tickSizeOuter(0));
  g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(opts.yt || 5).tickFormat(opts.yf || null).tickSizeOuter(0)).select(".domain").remove();
}
function title(g, text) { return g.append("text").attr("class", "title").attr("x", 0).attr("y", -14).text(text); }

// ---------- chapter 2: wage gradient -> decomposition ----------
function initWage() {
  const { svg, w, h } = svgBox("#g-wage");
  const m = { t: 40, r: 40, b: 40, l: 100 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const t1 = state.data.results.t1;
  const y = d3.scaleBand().domain(t1.map(d => d.eng)).range([0, ch * 0.58]).padding(0.45);
  const x = d3.scaleLinear().domain([0, 42]).range([0, cw]);
  axes(g, x, y, cw, ch * 0.58, { xf: d => "$" + d, xt: 6 });
  title(g, "Median hourly wage of foreign-born workers by English, 2022–2024");
  const rows = g.selectAll(".row").data(t1).join("g").attr("class", "row").attr("transform", d => `translate(0,${y(d.eng) + y.bandwidth() / 2})`);
  rows.append("line").attr("x1", d => x(d.lo)).attr("x2", d => x(d.hi)).attr("stroke", C.ink2).attr("stroke-width", 2).style("opacity", 0);
  rows.append("rect").attr("x", 0).attr("y", -y.bandwidth() / 2).attr("height", y.bandwidth()).attr("width", 0).attr("fill", (d, i) => C.english[i]);
  rows.append("text").attr("class", "label").attr("x", d => x(d.hi) + 8).attr("dy", 4).text(d => fmt$(d.median)).style("opacity", 0);
  hover(rows, d => `<b>${d.eng}</b><br>median ${fmt$(d.median)}<br>90% interval ${fmt$(d.lo)} to ${fmt$(d.hi)}`);
  const ann = g.append("text").attr("class", "label").attr("x", cw).attr("y", ch * 0.58 + 40).attr("text-anchor", "end").style("opacity", 0)
    .text("holding cohort, education, occupation and area constant, 0.059 log points per step");
  // decomposition bars
  const oa = state.data.results.oaxaca;
  const dg = g.append("g").attr("transform", `translate(0,${ch * 0.8})`).style("opacity", 0);
  const dy = d3.scaleBand().domain(["Standard controls", "With English"]).range([0, ch * 0.2]).padding(0.4);
  const dx = d3.scaleLinear().domain([0, oa.gap]).range([0, cw]);
  dg.append("text").attr("class", "title").attr("y", -12).text(`Native to foreign-born log wage gap, ${oa.gap.toFixed(3)}`);
  const parts = [["Standard controls", oa.base], ["With English", oa.with_english]];
  parts.forEach(([k, v]) => {
    const yy = dy(k);
    dg.append("rect").attr("x", 0).attr("y", yy).attr("height", dy.bandwidth()).attr("width", dx(v.explained)).attr("fill", C.teal);
    dg.append("rect").attr("x", dx(v.explained)).attr("y", yy).attr("height", dy.bandwidth()).attr("width", dx(v.unexplained)).attr("fill", C.grey2);
    dg.append("text").attr("x", -8).attr("y", yy + dy.bandwidth() / 2 + 4).attr("text-anchor", "end").text(k);
    dg.append("text").attr("class", "label").attr("x", dx(v.explained) / 2).attr("y", yy + dy.bandwidth() / 2 + 4).attr("text-anchor", "middle").attr("fill", "#fff").text(`explained ${Math.round(v.explained / oa.gap * 100)}%`);
    dg.append("text").attr("class", "note").attr("x", dx(v.explained) + dx(v.unexplained) / 2).attr("y", yy + dy.bandwidth() / 2 + 4).attr("text-anchor", "middle").text(`unexplained ${Math.round(v.unexplained / oa.gap * 100)}%`);
  });
  state.charts.wage = {
    step(i) {
      rows.select("rect").transition().duration(T).ease(ease).delay((d, j) => j * 120).attr("width", d => x(d.median));
      rows.select("text").transition().delay(500).duration(400).style("opacity", 1);
      rows.select("line").transition().delay(600).duration(400).style("opacity", i >= 0 ? 1 : 0);
      ann.transition().duration(500).style("opacity", i >= 1 ? 1 : 0);
      dg.transition().duration(600).style("opacity", i >= 2 ? 1 : 0);
      caption("#c-wage", i < 2 ? "Weighted medians with 90 percent replicate-weight intervals. Wage and salary workers aged 25 to 64."
        : "Two-fold Oaxaca decomposition with pooled coefficients, 2022–2024.");
    }
  };
}

// ---------- chapter 4b: county roses ----------
function initCounty() {
  const { svg, w, h } = svgBox("#s-county");
  if (!state.data.counties) { state.charts.county = { step() {} }; return; }
  const chart = CH.countyRose(svg, state.data.counties, { width: w, height: h });
  state.charts.county = {
    step(i) {
      chart.select(["Philadelphia", "Salem", "Chester"][i] || "Philadelphia");
      caption("#c-county", "Five percentages for the eleven counties of the metropolitan area, 2022–2024, on one 0 to 100 scale. The wage petal is the foreign-born median hourly wage as a percentage of the native-born median. Descriptive statistics for the same sample.");
    }
  };
}

// ---------- shared: dot-and-interval chart ----------
function intervalChart(g, rows, cw, ch, opts) {
  const y = d3.scaleBand().domain(rows.map(d => d.g)).range([0, ch]).padding(0.5);
  const x = d3.scaleLinear().domain(opts.domain).range([0, cw]);
  g.append("g").attr("class", "grid").call(d3.axisLeft(y).tickSize(-cw).tickFormat(""));
  g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).ticks(6).tickFormat(opts.xf).tickSizeOuter(0));
  g.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("y1", 0).attr("y2", ch).attr("stroke", C.ink).attr("stroke-width", 1);
  const r = g.selectAll(".iv").data(rows).join("g").attr("class", "iv").attr("transform", d => `translate(0,${y(d.g) + y.bandwidth() / 2})`);
  r.append("text").attr("x", -12).attr("dy", 4).attr("text-anchor", "end").attr("class", "label").text(d => d.g);
  r.append("line").attr("x1", d => x(d.v - 1.96 * d.se)).attr("x2", d => x(d.v + 1.96 * d.se)).attr("stroke", d => d.c).attr("stroke-width", 3).attr("stroke-linecap", "round");
  r.append("circle").attr("cx", d => x(d.v)).attr("r", 6).attr("fill", d => d.c).attr("stroke", "#fff").attr("stroke-width", 1.5);
  r.append("text").attr("class", "label").attr("x", d => x(d.v + 1.96 * d.se) + 10).attr("dy", 4).text(d => opts.lf(d));
  hover(r, d => `<b>${d.g}</b><br>${(d.v * 100).toFixed(1)}% per SD, SE ${(d.se * 100).toFixed(1)}<br>95% interval ${((d.v - 1.96 * d.se) * 100).toFixed(1)}% to ${((d.v + 1.96 * d.se) * 100).toFixed(1)}%`);
  return r;
}

// ---------- chapter 5: density slopes by group ----------
function initSlopes() {
  if (state.pumas) { const sb = svgBox("#s-scatter"); const sc = CH.linkedMap(sb.svg, state.pumas, { width: sb.w, height: sb.h, mode: "scatter" }); sc.trend(true); }
  const showScatter = on => { d3.select("#s-scatter").classed("off", !on); d3.select("#g-slopes").classed("off", on); };
  showScatter(true);
  const { svg, w, h } = svgBox("#g-slopes");
  const m = { t: 44, r: 110, b: 40, l: 210 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const S = state.data.results.slopes;
  const ttl = g.append("text").attr("class", "title").attr("y", -16).text("Change in log wage per SD of area immigrant density");
  const body = g.append("g");
  const lf = d => `${(d.v * 100).toFixed(1)}% (SE ${(d.se * 100).toFixed(1)})`;
  function draw(rows, hl) {
    body.selectAll("*").remove();
    const r = intervalChart(body, rows, cw, Math.min(ch, rows.length * 90), { domain: [-0.14, 0.04], xf: d => (d * 100).toFixed(0) + "%", lf });
    if (hl) r.style("opacity", d => hl.includes(d.g) ? 1 : 0.35);
  }
  const base = [{ g: "Proficient immigrants", ...S.generic[0], c: C.teal }, { g: "Limited-English immigrants", ...S.generic[1], c: C.orange }];
  const full = base.concat([{ g: "Native-born workers", ...S.generic[2], c: C.grey }, { g: "Proficient immigrants, with native wage control", ...S.with_native_wage[0], c: C.tealDark }]);
  state.charts.slopes = {
    step(i) {
      showScatter(i === 0);
      if (i === 1) draw(base, ["Proficient immigrants"]);
      else if (i === 2) draw(base);
      else if (i === 3) draw(full);
    }
  };
}

// ---------- chapter 7: own-language density ----------
function initOwnlang() {
  const { svg, w, h } = svgBox("#g-ownlang");
  const m = { t: 44, r: 120, b: 40, l: 230 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const O = state.data.results.ownlang;
  g.append("text").attr("class", "title").attr("y", -16).text("Change in log wage per SD of own-language density");
  const body = g.append("g");
  const lf = d => `${(d.v * 100).toFixed(1)}% (SE ${(d.se * 100).toFixed(1)})`;
  const rowsFor = (city, tag) => [{ g: `${tag}proficient`, v: O[city].prof, se: O[city].prof_se, c: C.teal }, { g: `${tag}limited English`, v: O[city].lim, se: O[city].lim_se, c: C.orange }];
  function draw(rows, dim) {
    body.selectAll("*").remove();
    const r = intervalChart(body, rows, cw, Math.min(ch, rows.length * 80), { domain: [-0.1, 0.04], xf: d => (d * 100).toFixed(0) + "%", lf });
    if (dim) r.style("opacity", 0.25);
  }
  state.charts.ownlang = {
    step(i) {
      if (i === 0) { draw(rowsFor("Philadelphia", "Philadelphia, "), true); caption("#c-ownlang", `Exploratory. ${fmtN(O.Philadelphia.n)} workers with a non-English home language. One SD of own-language density is ${O.Philadelphia.sd_pp} percentage points.`); }
      else if (i === 1) { draw(rowsFor("Philadelphia", "Philadelphia, ")); caption("#c-ownlang", `Interaction ${O.Philadelphia.inter} (SE ${O.Philadelphia.inter_se}). Generic density is held constant in the same model.`); }
      else { draw(rowsFor("Philadelphia", "Philadelphia, ").concat(rowsFor("New York", "New York, "))); caption("#c-ownlang", `New York: ${fmtN(O["New York"].n)} workers, one SD is ${O["New York"].sd_pp} points, interaction ${O["New York"].inter} (SE ${O["New York"].inter_se}).`); }
    }
  };
}

// ---------- chapter 9: pseudo-panel and duration-matched ----------
function initPanel() {
  const { svg, w, h } = svgBox("#g-panel");
  const m = { t: 40, r: 40, b: 44, l: 60 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const cells = state.data.results.cells.map(d => ({ ...d, suburb_prop: +d.suburb_prop, median_hourly: +d.median_hourly }));
  const D = state.data.results.duration;
  const A = g.append("g"), B = g.append("g").style("opacity", 0);
  // A: trajectories
  const x = d3.scaleLinear().domain([10, 45]).range([0, cw]), y = d3.scaleLinear().domain([50, 90]).range([ch, 0]);
  axes(A, x, y, cw, ch, { xf: d => "$" + d, yf: d => d + "%" });
  A.append("text").attr("class", "title").attr("y", -14).text("Suburban residence and median wage by cohort and region, three windows");
  A.append("text").attr("x", cw / 2).attr("y", ch + 36).attr("text-anchor", "middle").text("median hourly wage, 2024 dollars");
  const byCell = d3.groups(cells, d => d.cell).map(([k, v]) => ({ k, region: v[0].region, cohort: v[0].cohort, pts: v.sort((a, b) => a.window.localeCompare(b.window)) }));
  const line = d3.line().x(d => x(d.median_hourly)).y(d => y(d.suburb_prop));
  const paths = A.selectAll(".tr").data(byCell).join("path").attr("class", "tr").attr("fill", "none").attr("stroke", d => C.region[d.region]).attr("stroke-width", d => d.cohort === "2010s" ? 2.6 : 1.4).attr("stroke-opacity", 0.9).attr("d", d => line(d.pts));
  hover(paths, d => `<b>${d.region}, arrived ${d.cohort === "pre1990" ? "before 1990" : d.cohort}</b><br>` + d.pts.map(p => `${p.window}: ${fmt$(p.median_hourly)}, ${p.suburb_prop.toFixed(0)}% suburban`).join("<br>"));
  paths.on("mouseenter", function () { paths.attr("stroke-opacity", 0.15); d3.select(this).attr("stroke-opacity", 1).attr("stroke-width", 3.2); })
       .on("mouseleave.restore", function (ev, d) { paths.attr("stroke-opacity", 0.9).attr("stroke-width", d => d.cohort === "2010s" ? 2.6 : 1.4); });
  A.selectAll(".end").data(byCell).join("circle").attr("cx", d => x(d.pts[d.pts.length - 1].median_hourly)).attr("cy", d => y(d.pts[d.pts.length - 1].suburb_prop)).attr("r", d => d.cohort === "2010s" ? 5 : 3).attr("fill", d => C.region[d.region]);
  const leg = A.append("g").attr("transform", `translate(${cw - 120},${10})`);
  Object.entries(C.region).forEach(([k, c], i) => { leg.append("line").attr("x1", 0).attr("x2", 16).attr("y1", i * 18).attr("y2", i * 18).attr("stroke", c).attr("stroke-width", 2.5); leg.append("text").attr("x", 22).attr("y", i * 18 + 4).text(k); });
  A.append("text").attr("class", "note").attr("x", 0).attr("y", ch + 36).text("thick lines: 2010s arrivals");
  // B: duration-matched dumbbells
  const yb = d3.scaleBand().domain(D.map(d => d.region)).range([0, Math.min(ch, 320)]).padding(0.5);
  const xb = d3.scaleLinear().domain([50, 80]).range([0, cw]);
  B.append("text").attr("class", "title").attr("y", -14).text("Percent living outside the city at matched duration of residence");
  B.append("g").attr("class", "grid").call(d3.axisLeft(yb).tickSize(-cw).tickFormat(""));
  B.append("g").attr("class", "axis").attr("transform", `translate(0,${yb.range()[1]})`).call(d3.axisBottom(xb).ticks(6).tickFormat(d => d + "%").tickSizeOuter(0));
  const rb = B.selectAll(".db").data(D).join("g").attr("transform", d => `translate(0,${yb(d.region) + yb.bandwidth() / 2})`);
  rb.append("text").attr("x", -12).attr("dy", 4).attr("text-anchor", "end").attr("class", "label").text(d => d.region);
  rb.append("line").attr("x1", d => xb(d.c2000)).attr("x2", d => xb(d.c2010)).attr("stroke", C.grey2).attr("stroke-width", 4);
  rb.append("circle").attr("cx", d => xb(d.c2000)).attr("r", 6).attr("fill", C.grey);
  rb.append("circle").attr("cx", d => xb(d.c2010)).attr("r", 6).attr("fill", d => Math.abs(d.diff / d.se) >= 1.96 ? C.orange : C.gold);
  rb.append("text").attr("class", "label").attr("x", d => xb(Math.max(d.c2000, d.c2010)) + 12).attr("dy", 4).text(d => `${d.diff > 0 ? "+" : ""}${d.diff} points (SE ${d.se})`);
  const lb = B.append("g").attr("transform", `translate(0,${yb.range()[1] + 50})`);
  lb.append("circle").attr("r", 5).attr("fill", C.grey); lb.append("text").attr("x", 10).attr("dy", 4).text("2000s cohort in 2012–2016");
  lb.append("circle").attr("cx", 230).attr("r", 5).attr("fill", C.orange); lb.append("text").attr("x", 240).attr("dy", 4).text("2010s cohort in 2022–2024");
  state.charts.panel = {
    step(i) {
      if (i === 0) { A.transition().duration(500).style("opacity", 1); B.transition().duration(500).style("opacity", 0); drawPath(paths, 1200); caption("#c-panel", "Sixteen cohort-by-region cells, 44 cell-window observations. Each path runs from 2012–2016 to 2022–2024."); }
      else { A.transition().duration(500).style("opacity", 0); B.transition().duration(500).style("opacity", 1); caption("#c-panel", "Replicate-weight standard errors. Orange marks differences of about two standard errors or more."); }
    }
  };
}

// ---------- chapters whose graphic is the interactive panel from explore.js ----------
function setCtl(id, value, ev = "change") { const el = document.getElementById(id); if (!el) return; if (el.type === "checkbox") el.checked = !!value; else el.value = value; el.dispatchEvent(new Event(ev)); }
function initInteractive() {
  state.charts.globe = { step(i) { caption("#c-globe", i === 0 ? "Residents of the metropolitan area born outside the United States by country of birth, 2022–2024, all ages. Line width and color are the count. Hover a country, drag to turn the globe." : i === 1 ? "Historical and combined ACS codes are mapped to a present-day country. Codes without a boundary are counted in the total but not drawn." : "The foreign-born count is for the city of Philadelphia, from one-year ACS estimates."); } };
  state.charts.cohort = { step(i) { setCtl("co-t", i === 0 ? 10 : 20, "input"); caption("#c-cohort", "Predicted wage gap to comparable natives by years since migration, from the cohort specification with English, occupation, PUMA and year fixed effects. Move the slider or switch cohorts off."); } };
  state.charts.map = { step(i) { setCtl("area-var", "puma_fb_prop"); setCtl("area-flows", i === 0 ? "none" : "city_to_suburb"); caption("#c-map", i === 0 ? "Foreign-born percentage of residents by PUMA, with the same 48 PUMAs as dots against foreign-born median wage. Hover either side, or drag a box across the scatter." : "Where foreign-born residents who left the city for a suburb in the past year arrived, 2022–2024. Circle size is the annual count."); } };
  state.charts.enclave = { step(i) { setCtl("calc-measure", i === 0 ? "generic" : "occ"); caption("#c-enclave", i === 0 ? "Fitted log wage by area immigrant density from the two-level model, no occupation controls. Move the slider to read both groups at any density." : "The same lines with occupation fixed effects. The distance between them is the within-occupation differential."); } };
  state.charts.occ = { step(i) { setCtl("occ-sort", i === 0 ? "Limited English" : "wage"); caption("#c-occ", i === 0 ? "Percentage of each group in twelve occupation groups, 2022–2024, sorted by the limited-English percentage. Color is the group's median hourly wage there. The right column divides the limited-English median by the proficient median." : "Sorted by the native-born median wage. Descriptive statistics for the model sample."); } };
  state.charts.compare = { step(i) { setCtl("inter-sort", "t"); setCtl("inter-bonf", i === 1); caption("#c-compare", i === 0 ? "Every limited-English × density interaction in the paper, sorted by |t|, with 95 percent intervals. Hover a point for the estimate, the standard error and the sample." : "Marks with a black rim clear the Bonferroni line for seventeen tests, |t| above 2.92."); } };
  state.charts.moves = { step(i) { setCtl("flow-window", "2022-2024"); setCtl("flow-group", i === 2 ? "nb" : "fb"); setCtl("flow-movers", i > 0); caption("#c-moves", i === 0 ? "Above, where foreign-born adults aged 25 to 64 lived a year earlier and where they live now, 2022–2024. Below, the relative risk of each move per unit of the chosen covariate, with replicate-weight intervals." : i === 1 ? "The same year with the 85.7 percent who did not move removed. Ribbon color is the movers' median hourly wage where the paper reports it." : "Native-born adults for comparison."); } };
  state.charts.grid = { step(i) { setCtl("cell-metric", i === 0 ? "median_hourly" : "suburb_prop"); caption("#c-grid", i === 0 ? "Forty-four cells of foreign-born adults aged 25 to 64, arrival cohort by region of birth, across the three windows. Real median hourly wage in 2024 dollars. Click a row for its three windows." : "Percentage living outside Philadelphia County. Filter regions with the buttons."); } };
}

// ---------- boot ----------
async function boot() {
  const [trend, origins, world, results] = await Promise.all([
    d3.csv("data/trend.csv"), d3.csv("data/origins.csv"), d3.json("data/land-110m.json"), d3.json("data/results.json")
  ]);
  state.data = { trend, origins, results };
  state.world = topojson.feature(world, world.objects.land);
  try { state.pumas = await d3.json("data/pumas.geojson"); } catch (e) { state.pumas = null; }
  try { state.data.mobility = await d3.json("data/mobility.json"); } catch (e) { state.data.mobility = null; }
  try { state.data.occupations = await d3.csv("data/occupations.csv"); } catch (e) { state.data.occupations = null; }
  try { state.data.counties = await d3.csv("data/county_profiles.csv"); } catch (e) { state.data.counties = null; }
  try { const mr = await d3.csv("data/move_rates.csv"); state.data.moveRates = mr.map(d => ({ ...d, fb: d.fb === "TRUE" || d.fb === "true", n: +d.n, pct: +d.pct })); }
  catch (e) { state.data.moveRates = state.data.mobility ? state.data.mobility.move_rates.map(d => ({ ...d, fb: true })) : []; }
  try { const ir = await d3.csv("data/interactions_region.csv"); state.data.interactionsRegion = ir.filter(d => d.Estimate).map(d => ({ region: d.sample, est: d.Estimate, se: d["Std. Error"] })); } catch (e) { state.data.interactionsRegion = []; }
  const inits = { wage: initWage, county: initCounty, slopes: initSlopes, ownlang: initOwnlang, panel: initPanel, interactive: initInteractive };
  function initAll() { Object.values(inits).forEach(f => f()); }
  initAll();
  const scroller = scrollama();
  scroller.setup({ step: ".step", offset: 0.55 }).onStepEnter(({ element }) => {
    d3.selectAll(".step").classed("is-active", false); d3.select(element).classed("is-active", true);
    const [ch, i] = element.dataset.step.split("-");
    if (state.charts[ch]) state.charts[ch].step(+i);
  });
  let rt; window.addEventListener("resize", () => { clearTimeout(rt); rt = setTimeout(() => { initAll(); scroller.resize(); }, 200); });
}
boot();
