// LeafInjuryR - viewport size and full-screen helpers (local only)
(function () {
  function send() {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('lir_vh', window.innerHeight);
      Shiny.setInputValue('lir_fs', !!(document.fullscreenElement || document.webkitFullscreenElement));
    }
  }
  $(document).on('shiny:connected', send);
  var t; window.addEventListener('resize', function () { clearTimeout(t); t = setTimeout(send, 150); });
  ['fullscreenchange', 'webkitfullscreenchange'].forEach(function (ev) {
    document.addEventListener(ev, function () { setTimeout(function () { send(); $(window).trigger('resize'); }, 120); });
  });
  window.lirFullscreen = function (id) {
    var el = document.getElementById(id); if (!el) return;
    if (document.fullscreenElement || document.webkitFullscreenElement) {
      (document.exitFullscreen || document.webkitExitFullscreen).call(document);
    } else if (el.requestFullscreen) { el.requestFullscreen(); }
    else if (el.webkitRequestFullscreen) { el.webkitRequestFullscreen(); }
  };
})();
