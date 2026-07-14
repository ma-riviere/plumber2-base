// Front-end behaviour for front. CSP-safe: no inline handlers, everything
// is wired through event delegation on document.

(function () {
    "use strict";

    // Attach the CSRF token to every htmx request when the page exposes one.
    document.addEventListener("htmx:configRequest", function (event) {
        var meta = document.querySelector('meta[name="csrf-token"]');
        if (meta && meta.content) {
            event.detail.headers["X-CSRF-Token"] = meta.content;
        }
    });

    // Server-rendered modals: content swapped into #modal-slot is shown as a
    // Bootstrap modal; closing it empties the slot so the next swap starts clean.
    document.addEventListener("htmx:afterSwap", function (event) {
        if (event.detail.target.id !== "modal-slot") return;
        var element = event.detail.target.querySelector(".modal");
        if (!element || !window.bootstrap) return;
        var modal = window.bootstrap.Modal.getOrCreateInstance(element);
        element.addEventListener("hidden.bs.modal", function () {
            modal.dispose();
            event.detail.target.innerHTML = "";
        }, { once: true });
        modal.show();
    });

    // Servers signal a completed modal action with an HX-Trigger: fb:close-modal
    // header; hide the open modal and reset its forms.
    document.body.addEventListener("fb:close-modal", function () {
        document.querySelectorAll(".modal.show").forEach(function (element) {
            element.querySelectorAll("form").forEach(function (form) { form.reset(); });
            element.querySelectorAll(".file-dropzone-file").forEach(function (label) {
                label.textContent = "";
            });
            var modal = window.bootstrap && window.bootstrap.Modal.getInstance(element);
            if (modal) modal.hide();
        });
    });

    // File dropzone (upload modal): a dashed target wrapping a hidden file
    // input. Clicking opens the picker natively; drag&drop assigns the dropped
    // files to the input and mirrors the chosen filename. Delegated so it works
    // for modals rendered at any time.
    function updateDropzoneName(input) {
        var zone = input.closest(".file-dropzone");
        if (!zone) return;
        var label = zone.querySelector(".file-dropzone-file");
        if (label) label.textContent = input.files && input.files.length ? input.files[0].name : "";
    }

    document.addEventListener("change", function (event) {
        var input = event.target.closest(".file-dropzone-input");
        if (input) updateDropzoneName(input);
    });

    document.addEventListener("dragover", function (event) {
        var zone = event.target.closest(".file-dropzone");
        if (!zone) return;
        event.preventDefault();
        zone.classList.add("is-dragover");
    });

    document.addEventListener("dragleave", function (event) {
        var zone = event.target.closest(".file-dropzone");
        if (!zone || zone.contains(event.relatedTarget)) return;
        zone.classList.remove("is-dragover");
    });

    document.addEventListener("drop", function (event) {
        var zone = event.target.closest(".file-dropzone");
        if (!zone) return;
        event.preventDefault();
        zone.classList.remove("is-dragover");
        var input = zone.querySelector(".file-dropzone-input");
        if (!input || !event.dataTransfer) return;
        input.files = event.dataTransfer.files;
        updateDropzoneName(input);
        input.dispatchEvent(new Event("change", { bubbles: true }));
    });

    // Live <output> mirror for range sliders (CSP-safe: no inline oninput).
    // Any input[type=range].live-slider writes its value into the element
    // named by data-value-target while dragging.
    document.addEventListener("input", function (event) {
        var slider = event.target;
        if (!slider.matches('input[type="range"].live-slider')) return;
        var target = document.getElementById(slider.getAttribute("data-value-target") || "");
        if (target) target.textContent = slider.value;
    });

    // 5xx and network failures are configured not to swap (see the htmx-config
    // meta tag); surface them as a toast instead of failing silently.
    function serverErrorToast() {
        showToast(document.body.getAttribute("data-server-error") || "Server error", "danger");
    }
    document.addEventListener("htmx:responseError", serverErrorToast);
    document.addEventListener("htmx:sendError", serverErrorToast);

    // Clipboard copy via delegation. Equivalent to shiny-base's copyToClipboard:
    // any element carrying data-clipboard-text copies it; data-clipboard-message
    // (optional) is surfaced as a success toast.
    document.addEventListener("click", function (event) {
        var trigger = event.target.closest("[data-clipboard-text]");
        if (!trigger) return;
        event.preventDefault();
        copyToClipboard(
            trigger.getAttribute("data-clipboard-text"),
            trigger.getAttribute("data-clipboard-message")
        );
    });

    function copyToClipboard(text, message) {
        navigator.clipboard.writeText(text).then(
            function () { if (message) showToast(message, "success"); },
            function () { showToast("Copy failed", "danger"); }
        );
    }

    function showToast(message, variant) {
        var container = document.getElementById("toasts");
        if (!container || !window.bootstrap) return;
        var toast = document.createElement("div");
        toast.className = "toast align-items-center text-bg-" + variant + " border-0";
        toast.setAttribute("role", "alert");
        toast.setAttribute("aria-live", "assertive");
        toast.setAttribute("aria-atomic", "true");

        var body = document.createElement("div");
        body.className = "toast-body";
        body.textContent = message;

        var wrapper = document.createElement("div");
        wrapper.className = "d-flex";
        wrapper.appendChild(body);
        toast.appendChild(wrapper);
        container.appendChild(toast);

        var instance = window.bootstrap.Toast.getOrCreateInstance(toast);
        toast.addEventListener("hidden.bs.toast", function () { toast.remove(); });
        instance.show();
    }
})();
