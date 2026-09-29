/* Shared rich charts used by both the Story (main.js) and Data (explore.js) views. D3 v7.
   Each builder takes an svg selection (already sized), the data it needs and options, and returns { step(i) } plus any controls. */
window.CH = (function () {
  const K = { ink: "#0d0d0d", ink2: "#595959", muted: "#7f7f7f", grid: "#ececec", axis: "#bababa", teal: "#00b0be", tealDark: "#0d7d87", tealPale: "#dff4f4",
    orange: "#ea801c", gold: "#c99b38", grey: "#a1a1a1", grey2: "#d4d4d4" };
  const fmt$ = d3.format("$,.0f"), fmtN = d3.format(","), fmt3 = d3.format("+.3f");
  const tip = d3.select("body").selectAll("div.tip").data([0]).join("div").attr("class", "tip").style("opacity", 0);
  function hover(sel, html) {
    sel.on("mousemove.tip", (ev, d) => tip.style("opacity", 1).html(html(d)).style("left", (ev.pageX + 14) + "px").style("top", (ev.pageY - 10) + "px"))
       .on("mouseleave.tip", () => tip.style("opacity", 0));
  }
  const isCity = p => p.STATE === "PA" && String(p.PUMA).startsWith("032");
  const T = 700;

  // ---------- 1. linked map + scatter of the 48 PUMAs ----------
  // opts: { stacked: bool, mobility: flows array (optional), width, height }
  function linkedMap(svg, G, opts = {}) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, stacked = !!opts.stacked, mode = opts.mode || "both";
    const feats = G.features; feats.forEach(f => { f.properties.city = isCity(f.properties); f.properties.pop = +f.properties.pop || 0; });
    let mapW = stacked ? w : w * 0.52, mapH = stacked ? h * 0.55 : h, scX = stacked ? 0 : w * 0.52, scY = stacked ? h * 0.55 : 0, scW = stacked ? w : w * 0.48, scH = stacked ? h * 0.45 : h;
    if (mode === "map") { mapW = w; mapH = h; } else if (mode === "scatter") { scX = 0; scY = 0; scW = w; scH = h; }
    const proj = d3.geoMercator().fitExtent([[10, 30], [mapW - 10, mapH - 10]], G), path = d3.geoPath(proj);
    const gM = svg.append("g").style("display", mode === "scatter" ? "none" : null), gS = svg.append("g").attr("transform", `translate(${scX},${scY})`).style("display", mode === "map" ? "none" : null);
    const V = { puma_fb_prop: ["foreign-born percentage of residents", d => d.toFixed(1) + "%", d3.interpolate("#dff4f4", "#0a4f55")],
      fb_limited_prop: ["limited English among the foreign-born", d => d.toFixed(1) + "%", d3.interpolate("#fbe4cf", "#9a4d00")],
      fb_median_wage: ["foreign-born median hourly wage", fmt$, d3.interpolate("#f6efd9", "#7a5a10")] };
    let key = "puma_fb_prop";
    const mapTitle = gM.append("text").attr("class", "title").attr("x", 10).attr("y", 18);
    const shapes = gM.append("g").selectAll("path").data(feats).join("path").attr("d", path).attr("stroke", "#fff").attr("stroke-width", 0.8).attr("fill", "#eee").style("cursor", "pointer");
    gM.append("path").datum({ type: "FeatureCollection", features: feats.filter(f => f.properties.city) }).attr("d", path).attr("fill", "none").attr("stroke", K.ink).attr("stroke-width", 1.3).attr("pointer-events", "none");
    const lg = gM.append("g").attr("transform", `translate(${mapW - 130},${mapH - 40})`);
    const flowsG = gM.append("g").attr("pointer-events", "none");
    // scatter
    const m = { t: 34, r: 20, b: 44, l: 54 }, cw = scW - m.l - m.r, ch = scH - m.t - m.b;
    const sg = gS.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const x = d3.scaleLinear().domain([0, d3.max(feats, f => +f.properties.puma_fb_prop) * 1.08]).range([0, cw]);
    const y = d3.scaleLinear().domain([10, d3.max(feats, f => +f.properties.fb_median_wage) * 1.08]).range([ch, 0]);
    const rS = d3.scaleSqrt().domain([0, d3.max(feats, f => +f.properties.fb_limited_prop)]).range([3, 14]);
    sg.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(5).tickSize(-cw).tickFormat(""));
    sg.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).ticks(6).tickFormat(d => d + "%").tickSizeOuter(0));
    sg.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(5).tickFormat(d => "$" + d).tickSizeOuter(0)).select(".domain").remove();
    sg.append("text").attr("x", cw / 2).attr("y", ch + 36).attr("text-anchor", "middle").text("foreign-born percentage of residents");
    sg.append("text").attr("class", "title").attr("x", 0).attr("y", -16).text(mode === "scatter" || stacked ? "Foreign-born median hourly wage by immigrant density, 48 PUMAs, r = −0.37" : "Median wage by density, r = −0.37");
    // regression line (least squares on the 48 points, drawn as the trend)
    const xs = feats.map(f => +f.properties.puma_fb_prop), ys = feats.map(f => +f.properties.fb_median_wage);
    const mx = d3.mean(xs), my = d3.mean(ys), b = d3.sum(xs.map((v, i) => (v - mx) * (ys[i] - my))) / d3.sum(xs.map(v => (v - mx) ** 2)), a = my - b * mx;
    const trend = sg.append("line").attr("x1", x(x.domain()[0])).attr("x2", x(x.domain()[1])).attr("y1", y(a + b * x.domain()[0])).attr("y2", y(a + b * x.domain()[1])).attr("stroke", K.orange).attr("stroke-dasharray", "4 4").attr("stroke-width", 1.5).style("opacity", 0);
    const dots = sg.append("g").selectAll("circle").data(feats).join("circle").attr("cx", f => x(+f.properties.puma_fb_prop)).attr("cy", f => y(+f.properties.fb_median_wage)).attr("r", f => rS(+f.properties.fb_limited_prop))
      .attr("fill", K.teal).attr("fill-opacity", 0.7).attr("stroke", f => f.properties.city ? K.ink : "#fff").attr("stroke-width", f => f.properties.city ? 1.5 : 1).style("cursor", "pointer");
    sg.append("text").attr("class", "note").attr("x", cw).attr("y", -4).attr("text-anchor", "end").text("dot size is limited English among the foreign-born · black rim is the city");
    const html = f => { const p = f.properties; return `<b>${p.name.replace(" PUMA", "")}</b><br>foreign-born ${(+p.puma_fb_prop).toFixed(1)}% · limited English ${(+p.fb_limited_prop).toFixed(1)}%<br>foreign-born median wage ${fmt$(+p.fb_median_wage)}<br>${p.city ? "Philadelphia County" : "suburban county"}`; };
    hover(shapes, html); hover(dots, html);
    let locked = null;
    function light(f) {
      shapes.attr("stroke", g => g === f ? K.orange : "#fff").attr("stroke-width", g => g === f ? 2.5 : 0.8).filter(g => g === f).raise();
      dots.attr("stroke", g => g === f ? K.orange : (g.properties.city ? K.ink : "#fff")).attr("stroke-width", g => g === f ? 3 : (g.properties.city ? 1.5 : 1)).attr("fill-opacity", g => f && g !== f ? 0.35 : 0.7);
      if (opts.onSelect) opts.onSelect(f ? f.properties : null);
    }
    shapes.on("mouseenter", (ev, f) => light(f)).on("mouseleave", () => light(locked)).on("click", (ev, f) => { locked = locked === f ? null : f; light(locked); });
    dots.on("mouseenter", (ev, f) => light(f)).on("mouseleave", () => light(locked)).on("click", (ev, f) => { locked = locked === f ? null : f; light(locked); });
    // brush on the scatter selects on the map
    const brush = d3.brush().extent([[0, 0], [cw, ch]]).on("brush end", ({ selection }) => {
      if (!selection) { shapes.attr("fill-opacity", 1); dots.attr("fill-opacity", 0.7); return; }
      const [[x0, y0], [x1, y1]] = selection;
      const inside = f => { const px = x(+f.properties.puma_fb_prop), py = y(+f.properties.fb_median_wage); return px >= x0 && px <= x1 && py >= y0 && py <= y1; };
      shapes.attr("fill-opacity", f => inside(f) ? 1 : 0.15); dots.attr("fill-opacity", f => inside(f) ? 0.9 : 0.15);
    });
    sg.append("g").attr("class", "brush").call(brush).lower();
    sg.select(".brush").raise(); dots.raise();
    function color(k) {
      key = k; const [lab, fmt, interp] = V[k];
      const sc = d3.scaleSequential(interp).domain(d3.extent(feats, f => +f.properties[k]));
      shapes.transition().duration(T).attr("fill", f => sc(+f.properties[k]));
      dots.transition().duration(T).attr("fill", f => sc(+f.properties[k]));
      mapTitle.text((stacked ? lab + " by PUMA, 2022–2024" : lab.charAt(0).toUpperCase() + lab.slice(1)));
      lg.selectAll("*").remove();
      const ls = d3.scaleLinear().domain(sc.domain()).range([0, 110]);
      d3.range(0, 111, 3).forEach(v => lg.append("rect").attr("x", v).attr("y", 0).attr("width", 3).attr("height", 8).attr("fill", sc(ls.invert(v))));
      lg.append("text").attr("x", 0).attr("y", 22).text(fmt(sc.domain()[0])); lg.append("text").attr("x", 110).attr("y", 22).attr("text-anchor", "end").text(fmt(sc.domain()[1]));
    }
    function showFlows(on, type = "city_to_suburb") {
      flowsG.selectAll("*").remove();
      if (!on || !opts.mobility) return;
      const rows = opts.mobility.filter(d => d.window === "2022-2024" && d.move_type === type);
      const byId = new Map(feats.map(f => [f.properties.STATE + f.properties.PUMA, f]));
      const rs = d3.scaleSqrt().domain([0, d3.max(rows, d => d.n)]).range([2, 16]);
      const src = type === "city_to_suburb" ? feats.filter(f => f.properties.city) : feats.filter(f => !f.properties.city);
      const origin = path.centroid({ type: "FeatureCollection", features: src });
      const items = rows.map(d => ({ ...d, f: byId.get(d.STATE + d.PUMA) })).filter(d => d.f);
      flowsG.append("g").selectAll("path").data(items).join("path").attr("d", d => { const c = path.centroid(d.f); return `M${origin[0]},${origin[1]} Q${(origin[0] + c[0]) / 2},${Math.min(origin[1], c[1]) - 30} ${c[0]},${c[1]}`; })
        .attr("fill", "none").attr("stroke", K.orange).attr("stroke-opacity", 0.5).attr("stroke-width", d => Math.max(0.6, rs(d.n) / 4));
      const c = flowsG.append("g").selectAll("circle").data(items).join("circle").attr("cx", d => path.centroid(d.f)[0]).attr("cy", d => path.centroid(d.f)[1]).attr("r", 0).attr("fill", K.orange).attr("fill-opacity", 0.75).attr("stroke", "#fff").attr("pointer-events", "all");
      c.transition().duration(T).attr("r", d => rs(d.n));
      hover(c, d => `<b>${d.f.properties.name.replace(" PUMA", "")}</b><br>${fmtN(d.n)} foreign-born adults a year ${type === "city_to_suburb" ? "arrived here from the city" : "arrived here from the suburbs"}, 2022–2024`);
    }
    color(key);
    return {
      color, showFlows, trend: on => trend.transition().duration(T).style("opacity", on ? 1 : 0),
      step(i) { if (i === 0) { color("puma_fb_prop"); trend.style("opacity", 0); showFlows(false); } else if (i === 1) { color("fb_median_wage"); trend.transition().duration(T).style("opacity", 1); showFlows(false); } else { color("puma_fb_prop"); showFlows(true); } }
    };
  }

  // ---------- 2. every limited-English × density interaction ----------
  // rows: [{ label, city, measure, occ, est, se, n, p }]
  function INTERACTIONS(regionRows) {
    const R = [
      { label: "Generic density, 2022–2024", city: "Philadelphia", measure: "generic", occ: false, est: 0.049, se: 0.029, n: 7355, p: 0.09, main: true },
      { label: "Generic density, 2017–2021", city: "Philadelphia", measure: "generic", occ: false, est: 0.050, se: 0.028, n: 11374, p: 0.10 },
      { label: "Co-ethnic density", city: "Philadelphia", measure: "co-ethnic", occ: false, est: -0.033, se: 0.024, n: 7355 },
      { label: "Own-language density", city: "Philadelphia", measure: "own-language", occ: false, est: 0.046, se: 0.022, n: 5732, p: 0.04 },
      { label: "Generic density, own-language subsample", city: "Philadelphia", measure: "generic", occ: false, est: 0.055, se: 0.031, n: 5732 },
      { label: "Generic density, native wage control", city: "Philadelphia", measure: "generic", occ: false, est: 0.059, se: 0.029, n: 7355, p: 0.05 },
      { label: "Generic density, within occupation", city: "Philadelphia", measure: "generic", occ: true, est: 0.007, se: 0.016, n: 7355 },
      { label: "Own-language density, within occupation", city: "Philadelphia", measure: "own-language", occ: true, est: 0.021, se: 0.013, n: 5732 },
      { label: "Generic density", city: "New York", measure: "generic", occ: false, est: 0.018, se: 0.015, n: 68058 },
      { label: "Co-ethnic density", city: "New York", measure: "co-ethnic", occ: false, est: 0.018, se: 0.009, n: 68058 },
      { label: "Own-language density", city: "New York", measure: "own-language", occ: false, est: 0.045, se: 0.009, n: 54089 },
      { label: "Generic density, within occupation", city: "New York", measure: "generic", occ: true, est: 0.029, se: 0.015, n: 68058 },
      { label: "Own-language density, within occupation", city: "New York", measure: "own-language", occ: true, est: 0.028, se: 0.007, n: 54089 }
    ];
    (regionRows || []).forEach(r => R.push({ label: `Generic density, ${r.region}-born, within occupation`, city: "Philadelphia", measure: "generic", occ: true, est: +r.est, se: +r.se, n: null, region: r.region }));
    R.forEach(r => { r.t = r.est / r.se; if (r.p == null) r.p = 2 * (1 - normCdf(Math.abs(r.t))); });
    return R;
  }
  function normCdf(z) { const t = 1 / (1 + 0.2316419 * z), d = 0.3989423 * Math.exp(-z * z / 2); return 1 - d * t * (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274)))); }

  function interactions(svg, rows, opts = {}) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, m = { t: 44, r: 30, b: 60, l: 60 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const y = d3.scaleLinear().domain([-0.12, 0.14]).range([ch, 0]);
    const shape = { generic: d3.symbolCircle, "co-ethnic": d3.symbolDiamond, "own-language": d3.symbolSquare };
    const rN = d3.scaleSqrt().domain([0, 70000]).range([70, 300]);
    const ttl = g.append("text").attr("class", "title").attr("y", -22).text("All " + rows.length + " limited-English × density interactions, 95 percent intervals");
    g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(6).tickSize(-cw).tickFormat(""));
    g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(6).tickFormat(d3.format("+.2f")).tickSizeOuter(0)).select(".domain").remove();
    g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", K.ink);
    const bonf = g.append("g").style("opacity", 0);
    const zB = 2.92; // two-sided 0.05 / 17 tests
    bonf.append("text").attr("class", "note").attr("x", cw).attr("y", -6).attr("text-anchor", "end").text("filled with a black rim: |t| > 2.92, the Bonferroni line for 17 tests");
    let order = "t";
    const x = d3.scalePoint().range([20, cw - 20]).padding(0.5);
    const items = g.append("g").selectAll("g.it").data(rows, d => d.city + d.label).join("g").attr("class", "it").style("cursor", "pointer");
    items.append("line").attr("stroke", d => d.city === "Philadelphia" ? K.teal : K.orange).attr("stroke-width", 2.2).attr("stroke-linecap", "round");
    items.append("path").attr("d", d => d3.symbol().type(shape[d.measure]).size(d.n ? rN(d.n) : 90)())
      .attr("fill", d => d.occ ? "#fff" : (d.city === "Philadelphia" ? K.teal : K.orange)).attr("stroke", d => d.city === "Philadelphia" ? K.teal : K.orange).attr("stroke-width", 2);
    const legend = g.append("g").attr("transform", `translate(0,${ch + 30})`);
    [["Philadelphia", K.teal], ["New York", K.orange]].forEach(([n, c], i) => { legend.append("circle").attr("cx", 6 + i * 110).attr("cy", 0).attr("r", 5).attr("fill", c); legend.append("text").attr("x", 16 + i * 110).attr("y", 4).text(n); });
    const row2 = cw < 900 ? 18 : 0, off = cw < 900 ? 0 : 236;
    [["generic", d3.symbolCircle], ["co-ethnic", d3.symbolDiamond], ["own-language", d3.symbolSquare]].forEach(([n, s], i) => { legend.append("path").attr("transform", `translate(${off + 6 + i * 120},${row2})`).attr("d", d3.symbol().type(s).size(60)()).attr("fill", K.grey); legend.append("text").attr("x", off + 16 + i * 120).attr("y", 4 + row2).text(n); });
    legend.append("path").attr("transform", `translate(${off + 366},${row2})`).attr("d", d3.symbol().type(d3.symbolCircle).size(60)()).attr("fill", "#fff").attr("stroke", K.grey).attr("stroke-width", 2); legend.append("text").attr("x", off + 376).attr("y", 4 + row2).text("hollow: within occupation · size: sample");
    hover(items, d => `<b>${d.city} · ${d.label}</b><br>interaction ${fmt3(d.est)} (SE ${d.se.toFixed(3)}) · t = ${d.t.toFixed(2)} · p ${d.p < 0.001 ? "< 0.001" : "= " + d.p.toFixed(2)}${d.n ? `<br>n = ${fmtN(d.n)}` : ""}${d.main ? "<br>main specification" : ""}`);
    function layout(o) {
      order = o;
      const sorted = rows.slice().sort((a, b) => o === "t" ? Math.abs(b.t) - Math.abs(a.t) : o === "est" ? b.est - a.est : (a.city + a.label).localeCompare(b.city + b.label));
      x.domain(sorted.map(d => d.city + d.label));
      items.transition().duration(T).attr("transform", d => `translate(${x(d.city + d.label)},0)`);
      items.select("line").transition().duration(T).attr("y1", d => y(d.est - 1.96 * d.se)).attr("y2", d => y(d.est + 1.96 * d.se));
      items.select("path").transition().duration(T).attr("transform", d => `translate(0,${y(d.est)})`);
    }
    function bonferroni(on) {
      bonf.transition().duration(T).style("opacity", on ? 1 : 0);
      items.select("path").transition().duration(T).attr("stroke", d => on ? (Math.abs(d.t) > zB ? K.ink : (d.city === "Philadelphia" ? K.teal : K.orange)) : (d.city === "Philadelphia" ? K.teal : K.orange)).attr("stroke-width", d => on && Math.abs(d.t) > zB ? 3 : 2);
      items.transition().duration(T).style("opacity", d => on && Math.abs(d.t) <= zB ? 0.35 : 1);
    }
    layout("t");
    return { layout, bonferroni, step(i) { if (i === 0) { layout("t"); bonferroni(false); } else { layout("t"); bonferroni(true); } } };
  }

  // ---------- 3. occupation × English group matrix ----------
  function occMatrix(svg, rows, opts = {}) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, groups = ["Native-born", "Proficient immigrants", "Limited English"];
    const m = { t: 54, r: 30, b: 30, l: 250 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    rows.forEach(r => { r.pct = +r.pct; r.median_hourly = +r.median_hourly; r.n = +r.n; r.pop = +r.pop; });
    const occs = Array.from(new Set(rows.map(r => r.occ_group)));
    const get = (o, gname) => rows.find(r => r.occ_group === o && r.group === gname);
    const y = d3.scaleBand().domain(occs).range([0, Math.min(ch, occs.length * 44)]).padding(0.15);
    const colsEnd = Math.min(cw - 120, 560);
    const x = d3.scalePoint().domain(groups).range([80, colsEnd - 70]);
    const rS = d3.scaleSqrt().domain([0, d3.max(rows, r => r.pct)]).range([0, y.bandwidth() * 0.95]);
    const wage = d3.scaleSequential(d3.interpolate("#f6efd9", "#7a5a10")).domain(d3.extent(rows, r => r.median_hourly));
    const ttl = g.append("text").attr("class", "title").attr("y", -34).text("Where each group works, 2022–2024");
    const sub = g.append("text").attr("class", "note").attr("y", -18).text("circle size is the percentage of the group in the occupation, color is the group's median hourly wage there");
    const short = { "Native-born": "Native-born", "Proficient immigrants": "Proficient", "Limited English": "Limited English" };
    groups.forEach(gn => g.append("text").attr("class", "label").attr("x", x(gn)).attr("y", -2).attr("text-anchor", "middle").text(short[gn]));
    const rowG = g.append("g").selectAll("g.row").data(occs, d => d).join("g").attr("class", "row");
    rowG.append("text").attr("x", -12).attr("y", y.bandwidth() / 2 + 4).attr("text-anchor", "end").attr("class", "label").text(d => d);
    rowG.append("line").attr("x1", 0).attr("x2", colsEnd + 40).attr("y1", y.bandwidth() / 2).attr("y2", y.bandwidth() / 2).attr("stroke", K.grid);
    const cells = rowG.selectAll("g.cell").data(o => groups.map(gn => get(o, gn)).filter(Boolean)).join("g").attr("class", "cell").attr("transform", d => `translate(${x(d.group)},${y.bandwidth() / 2})`);
    cells.append("circle").attr("r", 0).attr("fill", d => wage(d.median_hourly)).attr("stroke", "#fff").attr("stroke-width", 1).transition().duration(T).attr("r", d => rS(d.pct));
    cells.append("text").attr("dy", 4).attr("text-anchor", "middle").attr("class", "label").attr("fill", d => d.median_hourly > 32 ? "#fff" : K.ink).style("font-size", "11px").text(d => d.pct >= 3 ? d.pct.toFixed(0) + "%" : "");
    // ratio column: limited-English wage as a percentage of the proficient wage in the same occupation
    const ratioX = colsEnd + 40;
    g.append("text").attr("class", "label").attr("x", ratioX).attr("y", -14).attr("text-anchor", "middle").text("limited ÷");
    g.append("text").attr("class", "label").attr("x", ratioX).attr("y", -1).attr("text-anchor", "middle").text("proficient wage");
    const rt = rowG.append("text").attr("class", "label").attr("x", ratioX).attr("y", y.bandwidth() / 2 + 4).attr("text-anchor", "middle").style("font-family", "var(--mono)")
      .text(o => { const a = get(o, "Limited English"), b = get(o, "Proficient immigrants"); return a && b ? (100 * a.median_hourly / b.median_hourly).toFixed(0) + "%" : ""; });
    hover(cells, d => `<b>${d.group} · ${d.occ_group}</b><br>${d.pct.toFixed(1)}% of the group · median wage ${d3.format("$.2f")(d.median_hourly)}<br>n = ${fmtN(d.n)} respondents, ${fmtN(d.pop)} weighted`);
    // legend
    const lg = g.append("g").attr("transform", `translate(0,${y.range()[1] + 16})`);
    const ls = d3.scaleLinear().domain(wage.domain()).range([0, 110]);
    d3.range(0, 111, 3).forEach(v => lg.append("rect").attr("x", v).attr("y", 0).attr("width", 3).attr("height", 8).attr("fill", wage(ls.invert(v))));
    lg.append("text").attr("x", 0).attr("y", 22).text(fmt$(wage.domain()[0])); lg.append("text").attr("x", 110).attr("y", 22).attr("text-anchor", "end").text(fmt$(wage.domain()[1]));
    lg.append("text").attr("x", 130).attr("y", 8).attr("class", "note").text("median hourly wage of the group in the occupation");
    function sortBy(k) {
      const key = k === "wage" ? o => -(get(o, "Native-born") || {}).median_hourly : o => -(get(o, k) || {}).pct;
      const ord = occs.slice().sort((a, b) => key(a) - key(b)); y.domain(ord);
      rowG.transition().duration(T).attr("transform", d => `translate(0,${y(d)})`);
    }
    sortBy("Limited English");
    return { sortBy, step(i) { sortBy(i === 0 ? "Limited English" : "wage"); ttl.text(i === 0 ? "Where each group works, sorted by the limited-English percentage" : "The same occupations, sorted by native-born wage"); } };
  }

  // ---------- 4. one year of moves as a flow ----------
  // rates: rows of {window, fb, move_type, n, pct}; opts.window, opts.fb, opts.exclude (stayed)
  const MOVE = { stayed: "Same house", within_city: "Moved within the city", within_suburbs: "Moved within the suburbs", city_to_suburb: "City to suburb", suburb_to_city: "Suburb to city", from_other_state: "Arrived from another state", from_abroad: "Arrived from abroad" };
  const ORIGIN = { stayed: "same house", within_city: "city", within_suburbs: "suburbs", city_to_suburb: "city", suburb_to_city: "suburbs", from_other_state: "another state", from_abroad: "abroad" };
  const DEST = { stayed: "same house", within_city: "city", within_suburbs: "suburbs", city_to_suburb: "suburbs", suburb_to_city: "city", from_other_state: "city or suburbs", from_abroad: "city or suburbs" };
  const WAGE = { city_to_suburb: 33, suburb_to_city: 20, from_abroad: null }; // movers' median hourly wage where the paper reports it
  function moveFlow(svg, rates, opts = {}) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, m = { t: 44, r: 150, b: 20, l: 170 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const ttl = g.append("text").attr("class", "title").attr("y", -22);
    const note = g.append("text").attr("class", "note").attr("y", -6);
    const body = g.append("g");
    let cur = { window: opts.window || "2022-2024", fb: opts.fb !== false, exclude: !!opts.exclude };
    function draw() {
      let rows = rates.filter(d => d.window === cur.window && (String(d.fb) === String(cur.fb)) && (!cur.exclude || d.move_type !== "stayed")).map(d => ({ ...d }));
      if (cur.window === "2022-2024" && cur.fb) { // the paper reports arrivals from abroad landing in the city and the suburbs at 3,400 to 5,500 a year
        const ab = rows.find(r => r.move_type === "from_abroad");
        if (ab) { const sc = 3400 / 8900; rows = rows.filter(r => r !== ab).concat([{ ...ab, dest: "city", n: ab.n * sc, pct: ab.pct * sc, wage: 16 }, { ...ab, dest: "suburbs", n: ab.n * (1 - sc), pct: ab.pct * (1 - sc), wage: 22 }]); }
      }
      body.selectAll("*").remove();
      if (!rows.length) { ttl.text("No rows for this group and window"); note.text("native-born rates arrive with data/move_rates.csv"); return; }
      const total = d3.sum(rows, d => +d.n);
      const order = ["stayed", "within_suburbs", "within_city", "from_abroad", "from_other_state", "city_to_suburb", "suburb_to_city"];
      rows.sort((a, b) => order.indexOf(a.move_type) - order.indexOf(b.move_type));
      const left = ["same house", "city", "suburbs", "another state", "abroad"].filter(k => rows.some(r => ORIGIN[r.move_type] === k));
      const dest = r => r.dest || DEST[r.move_type];
      const right = ["same house", "city", "suburbs", "city or suburbs"].filter(k => rows.some(r => dest(r) === k));
      const gap = 14, yS = d3.scaleLinear().domain([0, total]).range([0, ch - gap * Math.max(left.length, right.length)]);
      const stack = (keys, f) => { let y0 = 0; return Object.fromEntries(keys.map(k => { const sel = rows.filter(r => f(r) === k), v = d3.sum(sel, r => +r.n), pct = d3.sum(sel, r => +r.pct); const o = [k, { y0, y1: y0 + yS(v), v, pct, cursor: y0 }]; y0 += yS(v) + gap; return o; })); };
      const L = stack(left, r => ORIGIN[r.move_type]), Rr = stack(right, dest);
      const ribbons = rows.map(r => { const a = L[ORIGIN[r.move_type]], b = Rr[dest(r)], hgt = yS(+r.n); const o = { r, ay0: a.cursor, ay1: a.cursor + hgt, by0: b.cursor, by1: b.cursor + hgt }; a.cursor += hgt; b.cursor += hgt; return o; });
      const wg = d => d.r.wage || WAGE[d.r.move_type];
      const col = d => d.r.move_type === "stayed" ? K.grey2 : wg(d) ? (wg(d) > 25 ? K.tealDark : K.orange) : K.grey;
      const area = d => `M0,${d.ay0} C${cw / 2},${d.ay0} ${cw / 2},${d.by0} ${cw},${d.by0} L${cw},${d.by1} C${cw / 2},${d.by1} ${cw / 2},${d.ay1} 0,${d.ay1} Z`;
      const rb = body.append("g").selectAll("path").data(ribbons).join("path").attr("d", area).attr("fill", col).attr("fill-opacity", 0.55).attr("stroke", "#fff").attr("stroke-width", 0.5).style("cursor", "pointer");
      rb.on("mouseenter", function () { rb.attr("fill-opacity", 0.18); d3.select(this).attr("fill-opacity", 0.9); }).on("mouseleave", () => rb.attr("fill-opacity", 0.55));
      hover(rb, d => `<b>${MOVE[d.r.move_type]}</b><br>${(+d.r.pct).toFixed(1)}% of ${cur.fb ? "foreign-born" : "native-born"} adults 25 to 64, ${fmtN(Math.round(+d.r.n))} a year<br>${wg(d) ? "movers' median hourly wage " + fmt$(wg(d)) : "median wage not reported in the paper"}`);
      const node = (obj, xx, anchor, dx) => Object.entries(obj).forEach(([k, v]) => { body.append("rect").attr("x", xx - (anchor === "end" ? 6 : 0)).attr("y", v.y0).attr("width", 6).attr("height", Math.max(1, v.y1 - v.y0)).attr("fill", K.ink); body.append("text").attr("class", "label").attr("x", xx + dx).attr("y", (v.y0 + v.y1) / 2 + 4).attr("text-anchor", anchor).text(`${k} ${v.pct.toFixed(1)}%`); });
      node(L, 0, "end", -12); node(Rr, cw, "start", 12);
      body.append("text").attr("class", "label").attr("x", -12).attr("y", -6).attr("text-anchor", "end").text("a year earlier");
      body.append("text").attr("class", "label").attr("x", cw + 12).attr("y", -6).text("now");
      ttl.text(`${cur.fb ? "Foreign-born" : "Native-born"} adults 25 to 64, one year of moves, ${cur.window.replace("-", "–")}${cur.exclude ? ", movers only" : ""}`);
      note.text(cur.exclude ? "percentages of all adults in the group · teal: movers' median wage above 25 dollars · orange: below · grey: not reported" : "percentages of all adults in the group");
    }
    draw();
    return { set(o) { Object.assign(cur, o); draw(); }, step(i) { if (i === 0) { cur = { window: "2022-2024", fb: true, exclude: false }; } else if (i === 1) { cur = { window: "2022-2024", fb: true, exclude: true }; } else { cur = { window: "2022-2024", fb: false, exclude: true }; } draw(); } };
  }

  return { linkedMap, interactions, INTERACTIONS, occMatrix, moveFlow, hover, K };
})();

/* ---------- 5. full-screen flow map ---------- */
(function () {
  const K = CH.K, fmtN = d3.format(",");
  const COORD = { "India": [78.9, 20.6], "Mexico": [-102.5, 23.6], "China": [104.2, 35.9], "Dominican Republic": [-70.2, 18.7], "Vietnam": [108.3, 14.1], "Jamaica": [-77.3, 18.1], "Philippines": [121.8, 12.9], "Korea": [127.8, 35.9], "Brazil": [-51.9, -14.2], "Ukraine": [31.2, 48.4], "Guatemala": [-90.2, 15.8], "Liberia": [-9.4, 6.4], "Bangladesh": [90.4, 23.7], "Haiti": [-72.3, 19.0], "Nigeria": [8.7, 9.1], "Colombia": [-74.3, 4.6], "Honduras": [-86.2, 15.2], "Russia": [105.3, 61.5], "Poland": [19.1, 51.9], "Germany": [10.4, 51.2], "Italy": [12.6, 41.9], "Canada": [-106.3, 56.1], "Ecuador": [-78.2, -1.8], "Pakistan": [69.3, 30.4], "USSR": [105.3, 61.5], "El Salvador": [-88.9, 13.8], "Cambodia": [104.9, 12.6], "Ghana": [-1.0, 7.9], "England": [-1.2, 52.4], "United Kingdom, Not Specified": [-3.4, 55.4], "Trinidad and Tobago": [-61.2, 10.7], "Taiwan": [121.0, 23.7], "Egypt": [30.8, 26.8], "Turkey": [35.2, 38.9], "Peru": [-75.0, -9.2], "Venezuela": [-66.6, 6.4], "Greece": [21.8, 39.1], "Kenya": [37.9, 0.0], "Morocco": [-7.1, 31.8], "Guyana": [-58.9, 4.9], "Algeria": [1.7, 28.0], "Uzbekistan": [64.6, 41.4], "Albania": [20.2, 41.2], "Belarus": [27.9, 53.7], "Nepal": [84.1, 28.4], "Iran": [53.7, 32.4], "Ireland": [-8.2, 53.4], "Sierra Leone": [-11.8, 8.5], "Cuba": [-77.8, 21.5], "Thailand": [100.9, 15.9], "Laos": [102.5, 19.9], "Indonesia": [113.9, -0.8], "Israel": [34.9, 31.0], "Iraq": [43.7, 33.2], "Syria": [38.9, 34.8], "Ethiopia": [40.5, 9.1], "Sudan": [30.2, 12.9], "Portugal": [-8.2, 39.4], "France": [2.2, 46.2], "Spain": [-3.7, 40.5], "Argentina": [-63.6, -38.4], "Nicaragua": [-85.2, 12.9], "Costa Rica": [-83.8, 9.7], "Panama": [-80.8, 8.5], "Japan": [138.3, 36.2], "Hong Kong": [114.1, 22.4], "Burma": [95.9, 21.9], "Sri Lanka": [80.8, 7.9], "Afghanistan": [67.7, 33.9], "Romania": [24.9, 45.9], "Lithuania": [23.9, 55.2], "Moldova": [28.4, 47.4], "Bosnia and Herzegovina": [17.7, 43.9], "Barbados": [-59.5, 13.2], "Bahamas": [-77.4, 25.0], "Cameroon": [12.4, 7.4], "Togo": [0.8, 8.6], "Senegal": [-14.5, 14.5], "Mali": [-4.0, 17.6], "Ivory Coast": [-5.5, 7.5], "Eritrea": [39.8, 15.2], "Somalia": [46.2, 5.2], "Tanzania": [34.9, -6.4], "Uganda": [32.3, 1.4], "South Africa": [22.9, -30.6], "Zimbabwe": [29.2, -19.0], "Zambia": [27.8, -13.1], "Congo": [21.8, -4.0], "Australia": [133.8, -25.3], "New Zealand": [174.9, -40.9] };
  CH.flowMap = function (svg, G, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, feats = G.features;
    feats.forEach(f => { f.properties.city = f.properties.STATE === "PA" && String(f.properties.PUMA).startsWith("032"); });
    const proj = d3.geoMercator().fitExtent([[30, 30], [w * 0.74, h - 30]], G), path = d3.geoPath(proj);
    const byId = new Map(feats.map(f => [f.properties.STATE + f.properties.PUMA, f]));
    const cent = new Map(feats.map(f => [f.properties.STATE + f.properties.PUMA, path.centroid(f)]));
    // world layer: a globe with every country of birth colored by count and arcs to Philadelphia
    const gWorld = svg.append("g");
    const R0 = Math.min(w * 0.66, h - 200) / 2, wproj = d3.geoOrthographic().translate([w * 0.37, h / 2 + 20]).scale(R0).rotate([70, -25]).clipAngle(90), wpath = d3.geoPath(wproj);
    gWorld.append("path").datum({ type: "Sphere" }).attr("class", "sphere").attr("d", wpath).attr("fill", "#f4f6f7").attr("stroke", K.grey2);
    gWorld.append("path").datum(d3.geoGraticule10()).attr("class", "grat").attr("d", wpath).attr("fill", "none").attr("stroke", "#e4e7e9").attr("stroke-width", 0.5);
    const gCountries = gWorld.append("g"), gWArcs = gWorld.append("g").attr("pointer-events", "none"), gWDots = gWorld.append("g").attr("pointer-events", "none"), gWLab = gWorld.append("g").attr("pointer-events", "none");
    const wlegend = gWorld.append("g").attr("transform", `translate(30,${h - 50})`);
    const gMetro = svg.append("g").style("opacity", 0).style("pointer-events", "none");
    const shapes = gMetro.append("g").selectAll("path").data(feats).join("path").attr("d", path).attr("fill", "#f1f1ee").attr("stroke", "#fff").attr("stroke-width", 0.8);
    gMetro.append("path").datum({ type: "FeatureCollection", features: feats.filter(f => f.properties.city) }).attr("d", path).attr("fill", "none").attr("stroke", K.ink).attr("stroke-width", 1.3).attr("pointer-events", "none");
    const gLines = gMetro.append("g").attr("pointer-events", "none"), gDots = gMetro.append("g").attr("pointer-events", "none"), gLabel = gMetro.append("g").attr("pointer-events", "none");
    const legend = gMetro.append("g").attr("transform", `translate(30,80)`);
    function showMetro(on) {
      gMetro.transition().duration(600).style("opacity", on ? 1 : 0).style("pointer-events", on ? "all" : "none");
      gWorld.transition().duration(600).style("opacity", on ? 0 : 1).style("pointer-events", on ? "none" : "all");
    }
    let timer = null, particles = [], paths = [];
    function stopAnim() { if (timer) timer.stop(); timer = null; gDots.selectAll("*").remove(); gWDots.selectAll("*").remove(); }
    function animate(items, color, layer = gDots) {
      stopAnim(); paths = items; gWDots.selectAll("*").remove();
      particles = [];
      items.forEach(it => { const L = it.el.getTotalLength(); const k = Math.min(60, Math.max(1, Math.round(it.n / it.per))); for (let i = 0; i < k; i++) particles.push({ it, L, t: Math.random() }); });
      const dots = layer.selectAll("circle").data(particles).join("circle").attr("r", 2.2).attr("fill", color).attr("fill-opacity", 0.9);
      timer = d3.timer(() => {
        particles.forEach(p => { p.t += 0.0025 * (240 / Math.max(120, p.L)); if (p.t > 1) p.t -= 1; });
        dots.attr("transform", p => { const pt = p.it.el.getPointAtLength(p.t * p.L); return `translate(${pt.x},${pt.y})`; });
      });
    }
    function edgePoint(lonlat) {
      // bearing from Philadelphia toward the country, then the point where that ray leaves the map frame
      const a = proj([-75.1652, 39.9526]), b = proj(lonlat), dx = b[0] - a[0], dy = b[1] - a[1], m = Math.hypot(dx, dy) || 1;
      const ux = dx / m, uy = dy / m, r = Math.hypot(w, h);
      return [Math.min(w * 0.76, Math.max(4, a[0] + ux * r)), Math.min(h - 4, Math.max(4, a[1] + uy * r))];
    }
    function curve(p0, p1, bend = 0.25) { const mx = (p0[0] + p1[0]) / 2, my = (p0[1] + p1[1]) / 2, dx = p1[0] - p0[0], dy = p1[1] - p0[1]; return `M${p0[0]},${p0[1]} Q${mx - dy * bend},${my + dx * bend} ${p1[0]},${p1[1]}`; }
    function legendBar(sc, fmt, label) {
      legend.selectAll("*").remove();
      const ls = d3.scaleLinear().domain(sc.domain()).range([0, 140]);
      d3.range(0, 141, 4).forEach(v => legend.append("rect").attr("x", v).attr("y", 0).attr("width", 4).attr("height", 8).attr("fill", sc(ls.invert(v))));
      legend.append("text").attr("x", 0).attr("y", 22).text(fmt(sc.domain()[0])); legend.append("text").attr("x", 140).attr("y", 22).attr("text-anchor", "end").text(fmt(sc.domain()[1]));
      legend.append("text").attr("x", 156).attr("y", 8).attr("class", "note").text(label);
    }
    const ALIAS = { "Korea": "South Korea", "USSR": "Russia", "Dominican Republic": "Dominican Rep.", "England": "United Kingdom", "Scotland": "United Kingdom", "Northern Ireland": "United Kingdom", "United Kingdom, Not Specified": "United Kingdom",
      "Bosnia and Herzegovina": "Bosnia and Herz.", "Ivory Coast": "Côte d'Ivoire", "Democratic Republic of Congo (Zaire)": "Dem. Rep. Congo", "Czech Republic": "Czechia", "Czechoslovakia": "Czechia", "Yugoslavia": "Serbia", "Hong Kong": "China" };
    const byAtlas = new Map();
    d3.rollups(opts.origins, v => d3.sum(v, d => +d.pop), d => d.country).forEach(([label, pop]) => { const key = ALIAS[label] || label; if (!byAtlas.has(key)) byAtlas.set(key, { pop: 0, labels: [] }); const e = byAtlas.get(key); e.pop += pop; e.labels.push([label, pop]); });
    let worldOn = false, rot = null;
    function redrawGlobe() { gWorld.selectAll("path.sphere, path.grat").attr("d", wpath); gCountries.selectAll("path").attr("d", wpath); gWArcs.selectAll("path").attr("d", d => wpath(d.line)); gWLab.selectAll("*").remove(); worldLabels(); }
    function worldLabels() {
      const phl = [-75.1652, 39.9526], p = wproj(phl);
      if (d3.geoDistance(phl, [-wproj.rotate()[0], -wproj.rotate()[1]]) < Math.PI / 2) { gWLab.append("circle").attr("cx", p[0]).attr("cy", p[1]).attr("r", 5).attr("fill", K.orange).attr("stroke", "#fff").attr("stroke-width", 1.5); gWLab.append("text").attr("class", "title").attr("x", p[0] - 10).attr("y", p[1] - 12).attr("text-anchor", "end").text("Philadelphia"); }
      gWLab.append("text").attr("class", "title").attr("x", 30).attr("y", 30).text(`${fmtN(d3.sum(Array.from(byAtlas.values()), d => d.pop))} foreign-born residents by country of birth, 2022–2024`);
      gWLab.append("text").attr("class", "note").attr("x", 30).attr("y", 50).text("color and line width are the count · hover for the number · drag to turn the globe");
    }
    function world() {
      showMetro(false); worldOn = true;
      const countries = opts.countries ? opts.countries.features : [];
      const phl = [-75.1652, 39.9526];
      const vals = Array.from(byAtlas.values(), d => d.pop);
      const sc = d3.scaleSequentialLog(d3.interpolate("#dff4f4", "#0a4f55")).domain([Math.max(50, d3.min(vals)), d3.max(vals)]);
      const cs = gCountries.selectAll("path").data(countries, d => d.id).join("path").attr("d", wpath)
        .attr("fill", d => byAtlas.has(d.properties.name) ? sc(byAtlas.get(d.properties.name).pop) : "#e6e6e2").attr("stroke", "#fff").attr("stroke-width", 0.5).style("cursor", "default");
      cs.filter(d => !byAtlas.has(d.properties.name)).on("mousemove.tip", null).on("mouseleave.tip", null);
      CH.hover(cs.filter(d => byAtlas.has(d.properties.name)), d => { const e = byAtlas.get(d.properties.name); return e ? `<b>${d.properties.name}</b><br>${fmtN(Math.round(e.pop))} residents of the metropolitan area${e.labels.length > 1 ? "<br>" + e.labels.map(l => `${l[0]} ${fmtN(Math.round(l[1]))}`).join(" · ") : ""}` : ""; });
      cs.on("mouseenter", function (ev, d) { if (!byAtlas.has(d.properties.name)) return; arcs.attr("stroke-opacity", a => a.name === d.properties.name ? 0.95 : 0.08); }).on("mouseleave", () => arcs.attr("stroke-opacity", 0.35))
        ;
      const items = countries.filter(d => byAtlas.has(d.properties.name)).map(d => ({ name: d.properties.name, pop: byAtlas.get(d.properties.name).pop, line: { type: "LineString", coordinates: [d3.geoCentroid(d), phl] } })).sort((a, b) => b.pop - a.pop);
      const wS = d3.scaleSqrt().domain([0, items[0].pop]).range([0.5, 7]);
      const arcs = gWArcs.selectAll("path").data(items, d => d.name).join("path").attr("d", d => wpath(d.line)).attr("fill", "none").attr("stroke", K.tealDark).attr("stroke-opacity", 0.35).attr("stroke-width", d => wS(d.pop)).attr("stroke-linecap", "round");
      gWLab.selectAll("*").remove(); worldLabels();
      wlegend.selectAll("*").remove();
      const ls = d3.scaleLog().domain(sc.domain()).range([0, 140]);
      d3.range(0, 141, 4).forEach(v => wlegend.append("rect").attr("x", v).attr("y", 0).attr("width", 4).attr("height", 8).attr("fill", sc(ls.invert(v))));
      wlegend.append("text").attr("x", 0).attr("y", 22).text(fmtN(Math.round(sc.domain()[0]))); wlegend.append("text").attr("x", 140).attr("y", 22).attr("text-anchor", "end").text(fmtN(Math.round(sc.domain()[1])));
      wlegend.append("text").attr("x", 0).attr("y", -8).attr("class", "note").text("residents born in the country, log scale");
      // particles along the visible arcs
      animate(items.map((d, i) => ({ el: arcs.nodes()[i], n: d.pop, per: Math.max(600, items[0].pop / 40) })), K.tealDark, gWDots);
      // drag to rotate, slow turn until touched
      let r0, p0;
      svg.call(d3.drag().on("start", ev => { if (!worldOn) return; if (rot) rot.stop(); r0 = wproj.rotate(); p0 = [ev.x, ev.y]; })
        .on("drag", ev => { if (!worldOn) return; const k = 90 / R0; wproj.rotate([r0[0] + (ev.x - p0[0]) * k, Math.max(-60, Math.min(60, r0[1] - (ev.y - p0[1]) * k))]); redrawGlobe(); }));
      if (rot) rot.stop();
      rot = d3.timer(() => { if (!worldOn) { rot.stop(); return; } const r = wproj.rotate(); wproj.rotate([r[0] + 0.03, r[1]]); redrawGlobe(); });
      svg.on("mouseenter.rot", () => { if (rot) rot.stop(); });
    }
    function country(name, labels) {
      showMetro(true); worldOn = false; if (rot) rot.stop();
      const set = new Set(labels && labels.length ? labels : [name]);
      const rows = d3.rollups(opts.origins.filter(d => set.has(d.country)), v => ({ STATE: v[0].STATE, PUMA: v[0].PUMA, pop: d3.sum(v, d => +d.pop) }), d => d.STATE + d.PUMA).map(d => d[1]), total = d3.sum(rows, d => +d.pop);
      const pop = new Map(rows.map(d => [d.STATE + d.PUMA, +d.pop]));
      const fbPop = opts.fbByPuma;
      const sc = d3.scaleSequential(d3.interpolate("#dff4f4", "#0a4f55")).domain([0, d3.max(rows, d => +d.pop)]);
      shapes.transition().duration(600).attr("fill", f => pop.has(f.properties.STATE + f.properties.PUMA) ? sc(pop.get(f.properties.STATE + f.properties.PUMA)) : "#f1f1ee");
      CH.hover(shapes, f => { const id = f.properties.STATE + f.properties.PUMA, v = pop.get(id) || 0; return `<b>${f.properties.name.replace(" PUMA", "")}</b><br>${fmtN(v)} born in ${name}${fbPop && fbPop.get(id) ? ` · ${(100 * v / fbPop.get(id)).toFixed(0)}% of the PUMA's foreign-born` : ""}<br>${(100 * v / total).toFixed(1)}% of all ${name}-born in the metro`; });
      legendBar(sc, fmtN, `residents born in ${name}, 2022–2024`);
      gLines.selectAll("*").remove(); gLabel.selectAll("*").remove();
      const c = COORD[name];
      if (!c) { stopAnim(); return; }
      const src = edgePoint(c), top = rows.slice().sort((a, b) => b.pop - a.pop).slice(0, 25);
      const wS = d3.scaleSqrt().domain([0, top[0].pop]).range([0.4, 5]);
      const lines = gLines.selectAll("path").data(top).join("path").attr("d", d => curve(src, cent.get(d.STATE + d.PUMA), 0.18)).attr("fill", "none").attr("stroke", K.tealDark).attr("stroke-opacity", 0.35).attr("stroke-width", d => wS(+d.pop));
      gLabel.append("text").attr("class", "title").attr("x", 30).attr("y", 30).text(`Born in ${name}: ${fmtN(total)} residents of the metropolitan area, by PUMA of residence, 2022–2024`);
      gLabel.append("text").attr("class", "note").attr("x", 30).attr("y", 50).text("color is the count in each PUMA · lines enter from the direction of the country and reach the 25 largest PUMAs · hover an area");
      animate(top.map((d, i) => ({ el: lines.nodes()[i], n: +d.pop, per: Math.max(200, top[0].pop / 12) })), K.tealDark);
    }
    function moves(type) {
      showMetro(true); worldOn = false; if (rot) rot.stop();
      const rows = opts.flows.filter(d => d.window === "2022-2024" && d.move_type === type).map(d => ({ ...d, f: byId.get(d.STATE + d.PUMA) })).filter(d => d.f);
      const src = path.centroid({ type: "FeatureCollection", features: feats.filter(f => type === "city_to_suburb" ? f.properties.city : !f.properties.city) });
      const sc = d3.scaleSequential(d3.interpolate("#fbe4cf", "#9a4d00")).domain([0, d3.max(rows, d => d.n)]);
      const n = new Map(rows.map(d => [d.STATE + d.PUMA, d.n]));
      shapes.transition().duration(600).attr("fill", f => n.has(f.properties.STATE + f.properties.PUMA) ? sc(n.get(f.properties.STATE + f.properties.PUMA)) : "#f1f1ee");
      CH.hover(shapes, f => `<b>${f.properties.name.replace(" PUMA", "")}</b><br>${fmtN(n.get(f.properties.STATE + f.properties.PUMA) || 0)} foreign-born residents a year arrived ${type === "city_to_suburb" ? "from the city" : "from the suburbs"}, 2022–2024`);
      legendBar(sc, fmtN, type === "city_to_suburb" ? "arrivals a year from the city" : "arrivals a year from the suburbs");
      gLines.selectAll("*").remove(); gLabel.selectAll("*").remove();
      const wS = d3.scaleSqrt().domain([0, d3.max(rows, d => d.n)]).range([0.5, 6]);
      const lines = gLines.selectAll("path").data(rows).join("path").attr("d", d => curve(src, cent.get(d.STATE + d.PUMA), 0.3)).attr("fill", "none").attr("stroke", K.orange).attr("stroke-opacity", 0.4).attr("stroke-width", d => wS(d.n));
      gLabel.append("text").attr("class", "title").attr("x", 30).attr("y", 24).text(type === "city_to_suburb" ? `City to suburb, ${fmtN(d3.sum(rows, d => d.n))} foreign-born residents a year, 2022–2024` : `Suburb to city, ${fmtN(d3.sum(rows, d => d.n))} foreign-born residents a year, 2022–2024`);
      animate(rows.map((d, i) => ({ el: lines.nodes()[i], n: d.n, per: 40 })), K.orange);
    }
    return { world, country, moves, stop: stopAnim };
  };
})();

/* ---------- 6. county roses: five percentage measures on one 0 to 100 scale ---------- */
(function () {
  const K = CH.K, fmtN = d3.format(",");
  const M = [
    { k: "fb_pct", short: "foreign-born", label: "foreign-born, % of residents", c: "#0d7d87" },
    { k: "limited_pct_fb", short: "limited English", label: "limited English, % of the foreign-born", c: "#00b0be" },
    { k: "wage_ratio", short: "immigrant ÷ native wage", label: "foreign-born median hourly wage as % of the native-born median", c: "#ea801c" },
    { k: "fb_ba_pct", short: "with a degree", label: "foreign-born aged 25 and over with a bachelor's degree or more, %", c: "#c99b38" },
    { k: "fb_rent_burden", short: "rent burden", label: "foreign-born renters' median rent, % of income", c: "#a1a1a1" }
  ];
  CH.countyRose = function (svg, rows, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height;
    rows = rows.map(r => ({ ...r, name: r.county.replace(" County", ""), pop: +r.pop, n_fb: +r.n_fb, fb_pct: +r.fb_pct, limited_pct_fb: +r.limited_pct_fb, fb_ba_pct: +r.fb_ba_pct, fb_rent_burden: +r.fb_rent_burden,
      fb_median_wage: +r.fb_median_wage, nb_median_wage: +r.nb_median_wage, wage_ratio: 100 * +r.fb_median_wage / +r.nb_median_wage }));
    rows.sort((a, b) => b.pop - a.pop);
    const scale = d3.scaleLinear().domain([0, 100]);
    const ang = d3.scaleBand().domain(M.map(m => m.k)).range([0, 2 * Math.PI]).padding(0.06);
    const mid = m => ang(m.k) + ang.bandwidth() / 2;
    let selected = opts.selected || "Philadelphia";
    // layout: big rose on the left, ten small roses in a 5 x 2 grid on the right
    const bigR = Math.min(h * 0.27, w * 0.17), bigC = [Math.max(bigR + 130, w * 0.24), h * 0.55];
    const gridX = bigC[0] + bigR + 140, cols = w - gridX > 520 ? 5 : 2, rowsN = Math.ceil(10 / cols), cellW = (w - gridX - 10) / cols, cellH = Math.min(cellW * 1.1, (h - 110) / rowsN), smallR = Math.min(cellW, cellH) * 0.33, gridY = h * 0.55 - cellH * rowsN / 2;
    svg.append("text").attr("class", "title").attr("x", 20).attr("y", 24).text("Immigrant geography by county, 2022–2024");
    svg.append("text").attr("class", "note").attr("x", 20).attr("y", 42).text("foreign-born percentage, limited English, the immigrant to native wage ratio, degrees and rent burden, each 0 to 100 · click a county");
    const selTitle = svg.append("text").attr("class", "title").attr("x", 20).attr("y", 72).style("font-size", "18px"), selSub = svg.append("text").attr("class", "note").attr("x", 20).attr("y", 90);
    const node = svg.append("g").selectAll("g.rose").data(rows, d => d.name).join("g").attr("class", "rose").style("cursor", "pointer");
    node.append("g").attr("class", "body"); node.append("text").attr("class", "name label").attr("text-anchor", "middle");
    const rings = [20, 40, 60, 80, 100];
    function drawRose(g, r, R, big) {
      const body = g.select("g.body"); body.selectAll("*").remove();
      const rr = v => R * scale(v);
      body.selectAll("circle.ring").data(rings).join("circle").attr("class", "ring").attr("r", rr).attr("fill", "none").attr("stroke", K.grid).attr("stroke-width", big ? 0.8 : 0.5);
      body.selectAll("line.spoke").data(M).join("line").attr("class", "spoke").attr("x1", 0).attr("y1", 0).attr("x2", m => Math.sin(ang(m.k)) * R).attr("y2", m => -Math.cos(ang(m.k)) * R).attr("stroke", K.grid).attr("stroke-width", big ? 0.8 : 0.5);
      const p = body.selectAll("path.petal").data(M).join("path").attr("class", "petal").attr("fill", m => m.c).attr("stroke", "#fff").attr("stroke-width", big ? 1.2 : 0.6)
        .attr("d", m => d3.arc().innerRadius(0).outerRadius(rr(r[m.k])).startAngle(ang(m.k)).endAngle(ang(m.k) + ang.bandwidth())());
      body.selectAll("circle.cut").data(rings.slice(0, -1)).join("circle").attr("class", "cut").attr("r", rr).attr("fill", "none").attr("stroke", "#fff").attr("stroke-width", big ? 1.2 : 0.6).attr("pointer-events", "none");
      CH.hover(p, m => `<b>${r.name} County</b><br>${m.label}<br>${r[m.k].toFixed(1)}%${m.k === "wage_ratio" ? ` (${d3.format("$.2f")(r.fb_median_wage)} against ${d3.format("$.2f")(r.nb_median_wage)})` : ""} · rank ${rows.slice().sort((a, b) => b[m.k] - a[m.k]).findIndex(x => x === r) + 1} of ${rows.length}`);
      if (big) {
        body.selectAll("text.tick").data(rings).join("text").attr("class", "tick").attr("x", 4).attr("y", d => -rr(d) + 11).attr("fill", K.muted).style("font-size", "10.5px").text(d => d + "%");
        const lab = body.selectAll("g.lab").data(M).join("g").attr("class", "lab").attr("pointer-events", "none")
          .attr("transform", m => { const a = mid(m) * 180 / Math.PI, flip = a > 100 && a < 260; return `rotate(${a - 90}) translate(${R + 10},0) rotate(${flip ? 180 : 0})`; });
        const anchor = m => { const a = mid(m) * 180 / Math.PI; return a > 100 && a < 260 ? "end" : "start"; };
        lab.append("text").attr("text-anchor", anchor).attr("dy", -3).attr("fill", m => m.c).style("font-family", "var(--sans)").style("font-weight", 700).style("font-size", "12.5px").text(m => m.short);
        lab.append("text").attr("text-anchor", anchor).attr("dy", 12).attr("fill", K.ink).text(m => r[m.k].toFixed(1) + "%");
      }
    }
    function place(animate) {
      const others = rows.filter(r => r.name !== selected);
      node.each(function (r) {
        const g = d3.select(this), big = r.name === selected, R = big ? bigR : smallR;
        const i = others.indexOf(r);
        const [x, y] = big ? bigC : [gridX + (i % cols) * cellW + cellW / 2, gridY + Math.floor(i / cols) * cellH + cellH / 2];
        (animate ? g.transition().duration(700).ease(d3.easeCubicInOut) : g).attr("transform", `translate(${x},${y})`);
        drawRose(g, r, R, big);
        g.select("text.name").attr("y", R + 16).text(big ? "" : r.name);
        if (big) { selTitle.text(`${r.name} County`); selSub.text(`${fmtN(r.pop)} residents · ${fmtN(r.n_fb)} foreign-born respondents in the sample`); }
      });
    }
    function select(name) { if (name === selected) return; selected = name; place(true); }
    node.on("click", (ev, r) => select(r.name));
    place(false);
    return { select };
  };
})();
