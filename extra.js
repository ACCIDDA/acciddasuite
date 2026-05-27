(function () {
  function buildZoomableDiagram(container) {
    if (container.dataset.zoomableInitialized === "true") {
      return;
    }

    const widget = container.querySelector(".DiagrammeR, .html-widget");
    const svg = container.querySelector("svg");
    if (!widget || !svg) {
      return;
    }

    const toolbar = document.createElement("div");
    toolbar.className = "zoomable-diagram__toolbar";

    const hint = document.createElement("p");
    hint.className = "zoomable-diagram__hint";
    hint.textContent = "Zoom in, zoom out, reset, or expand to full screen.";

    const controls = document.createElement("div");
    controls.className = "zoomable-diagram__controls";

    const viewport = document.createElement("div");
    viewport.className = "zoomable-diagram__viewport";

    const zoomOut = document.createElement("button");
    zoomOut.type = "button";
    zoomOut.textContent = "Zoom out";

    const zoomIn = document.createElement("button");
    zoomIn.type = "button";
    zoomIn.textContent = "Zoom in";

    const reset = document.createElement("button");
    reset.type = "button";
    reset.textContent = "Reset";

    const fullscreen = document.createElement("button");
    fullscreen.type = "button";
    fullscreen.textContent = "Full screen";

    controls.append(zoomOut, zoomIn, reset, fullscreen);
    toolbar.append(hint, controls);

    container.insertBefore(toolbar, widget);
    container.appendChild(viewport);
    viewport.appendChild(widget);

    let scale = 1;

    function applyScale() {
      svg.style.transform = "scale(" + scale + ")";
    }

    function setScale(nextScale) {
      scale = Math.max(0.6, Math.min(2.4, nextScale));
      applyScale();
    }

    zoomOut.addEventListener("click", function () {
      setScale(scale - 0.2);
    });

    zoomIn.addEventListener("click", function () {
      setScale(scale + 0.2);
    });

    reset.addEventListener("click", function () {
      scale = 1;
      applyScale();
      viewport.scrollTop = 0;
      viewport.scrollLeft = 0;
    });

    fullscreen.addEventListener("click", function () {
      if (document.fullscreenElement === container) {
        document.exitFullscreen();
      } else if (container.requestFullscreen) {
        container.requestFullscreen();
      }
    });

    container.dataset.zoomableInitialized = "true";
    container.classList.add("is-ready");
    applyScale();
  }

  function initializeZoomableDiagrams() {
    document.querySelectorAll(".zoomable-diagram").forEach(buildZoomableDiagram);
  }

  document.addEventListener("DOMContentLoaded", initializeZoomableDiagrams);

  const observer = new MutationObserver(function () {
    initializeZoomableDiagrams();
  });

  observer.observe(document.documentElement, {
    childList: true,
    subtree: true
  });
})();
