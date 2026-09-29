/* Single-page routing: #story, #data, #methods, with an optional target such as #data/p-calc or #story/ch-moves. */
(function () {
  const views = ["story", "data", "methods"];
  function go() {
    const [view, target] = (location.hash.replace("#", "") || "story").split("/");
    const v = views.includes(view) ? view : "story";
    views.forEach(k => { document.getElementById("v-" + k).hidden = k !== v; });
    document.querySelectorAll(".rail a").forEach(a => a.classList.toggle("on", a.dataset.view === v));
    if (v === "data" && window.exploreBoot) window.exploreBoot();
    document.body.dataset.view = v;
    requestAnimationFrame(() => {
      window.dispatchEvent(new Event("resize"));
      const el = target && document.getElementById(target);
      if (el) el.scrollIntoView({ behavior: "smooth", block: "start" }); else window.scrollTo({ top: 0 });
    });
  }
  window.addEventListener("hashchange", go);
  go();
})();
