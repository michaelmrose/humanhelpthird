(ns net.humanhelp.components.phone-auth.scripts)

(def phone-auth-js
  "(function (global) {
  'use strict';

  var installed = false;

  function toArray(xs) {
    return Array.prototype.slice.call(xs || []);
  }

  function rawDigits(value) {
    return String(value || '').replace(/\\D/g, '');
  }

  function phoneDigits(value) {
    var digits = rawDigits(value);

    if (digits.length === 11 && digits.charAt(0) === '1') {
      digits = digits.slice(1);
    }

    return digits.slice(0, 10);
  }

  function formatDigits(digits) {
    digits = String(digits || '').slice(0, 10);

    if (digits.length === 0) return '';
    if (digits.length <= 2) return digits;
    if (digits.length === 3) return digits + '-';
    if (digits.length <= 5) return digits.slice(0, 3) + '-' + digits.slice(3);
    if (digits.length === 6) return digits.slice(0, 3) + '-' + digits.slice(3, 6) + '-';

    return digits.slice(0, 3) + '-' + digits.slice(3, 6) + '-' + digits.slice(6);
  }

  function formattedPhone(value) {
    return formatDigits(phoneDigits(value));
  }

  function digitCountBefore(value, caret) {
    return rawDigits(String(value || '').slice(0, caret)).length;
  }

  function caretAfterDigitCount(value, digitCount, wasAtEnd) {
    if (wasAtEnd) {
      return value.length;
    }

    if (digitCount <= 0) {
      return 0;
    }

    var seen = 0;

    for (var i = 0; i < value.length; i++) {
      if (/\\d/.test(value.charAt(i))) {
        seen += 1;

        if (seen >= digitCount) {
          if ((digitCount === 3 || digitCount === 6) && value.charAt(i + 1) === '-') {
            return i + 2;
          }

          return i + 1;
        }
      }
    }

    return value.length;
  }

  function removeDigitAt(digits, index) {
    if (index < 0 || index >= digits.length) {
      return digits;
    }

    return digits.slice(0, index) + digits.slice(index + 1);
  }

  function setCaret(display, caret) {
    try {
      display.setSelectionRange(caret, caret);
    } catch (_) {}
  }

  function setCaretToEnd(display) {
    setCaret(display, String(display.value || '').length);
  }

  function isDisplayInput(target) {
    return Boolean(target && target.matches && target.matches('[data-phone-auth-phone-display]'));
  }

  function rootFor(display) {
    if (display.closest) {
      return display.closest('[data-phone-auth-panel]') || display.form || global.document;
    }

    return display.form || global.document;
  }

  function hiddenInput(display) {
    var id = display.dataset && display.dataset.phoneAuthHiddenId;

    if (id && global.document && global.document.getElementById) {
      return global.document.getElementById(id);
    }

    var root = rootFor(display);

    return root && root.querySelector ? root.querySelector('[data-phone-auth-phone-hidden]') : null;
  }

  function errorElement(display) {
    var id = display.dataset && display.dataset.phoneAuthErrorId;

    if (id && global.document && global.document.getElementById) {
      return global.document.getElementById(id);
    }

    var root = rootFor(display);

    return root && root.querySelector ? root.querySelector('[data-phone-auth-error]') : null;
  }

  function requiredMessage(display) {
    return (display.dataset && display.dataset.phoneAuthRequiredMessage) ||
      'Please enter a 10-digit US mobile number.';
  }

  function invalidMessage(display) {
    return (display.dataset && display.dataset.phoneAuthInvalidMessage) ||
      'Please enter a 10-digit US mobile number.';
  }

  function validationMessage(display, digits) {
    if (digits.length === 0) {
      return requiredMessage(display);
    }

    if (digits.length !== 10) {
      return invalidMessage(display);
    }

    return '';
  }

  function setHiddenValue(display, digits) {
    var hidden = hiddenInput(display);

    if (hidden) {
      hidden.value = digits;
    }
  }

  function setDisplayValue(display, formatted) {
    if (display.value !== formatted) {
      display.value = formatted;
    }
  }

  function setError(display, message, show) {
    var error = errorElement(display);

    if (message) {
      if (display.setCustomValidity) {
        display.setCustomValidity(message);
      }

      if (display.setAttribute) {
        display.setAttribute('aria-invalid', 'true');
      }

      if (show && error) {
        error.textContent = message;

        if (error.classList && error.classList.remove) {
          error.classList.remove('hidden');
        }
      }
    } else {
      if (display.setCustomValidity) {
        display.setCustomValidity('');
      }

      if (display.setAttribute) {
        display.setAttribute('aria-invalid', 'false');
      }

      if (error) {
        error.textContent = '';

        if (error.classList && error.classList.add) {
          error.classList.add('hidden');
        }
      }
    }
  }

  function validate(display, show) {
    var digits = phoneDigits(display.value);
    var message = validationMessage(display, digits);

    setError(display, message, show || display.dataset.touched === 'true');

    return message === '';
  }

  function sync(display, opts) {
    opts = opts || {};

    if (!display || display.__phoneAuthSyncing) {
      return;
    }

    display.__phoneAuthSyncing = true;

    var before = display.value || '';
    var caret = display.selectionStart == null ? before.length : display.selectionStart;
    var wasAtEnd = caret === before.length;
    var digitPosition = digitCountBefore(before, caret);
    var digits = phoneDigits(before);
    var formatted = formatDigits(digits);

    setDisplayValue(display, formatted);
    setHiddenValue(display, digits);

    if (opts.caretToEnd) {
      setCaretToEnd(display);
    } else if (opts.preserveCaret) {
      setCaret(display, caretAfterDigitCount(formatted, digitPosition, wasAtEnd));
    }

    validate(display, opts.showError);

    display.__phoneAuthSyncing = false;
  }

  function selectedDigitCount(display) {
    var start = display.selectionStart == null ? 0 : display.selectionStart;
    var end = display.selectionEnd == null ? start : display.selectionEnd;

    return rawDigits(String(display.value || '').slice(start, end)).length;
  }

  function insertedDigitCount(event) {
    if (event.inputType === 'insertText') {
      return /^\\d$/.test(event.data || '') ? 1 : 0;
    }

    return 0;
  }

  function prevent(event) {
    if (event.preventDefault) {
      event.preventDefault();
    }

    event.defaultPrevented = true;
  }

  function handleBoundaryDelete(display, event) {
    var value = display.value || '';
    var start = display.selectionStart == null ? value.length : display.selectionStart;
    var end = display.selectionEnd == null ? start : display.selectionEnd;

    if (start !== end) {
      return false;
    }

    var removeIndex = null;

    if (event.inputType === 'deleteContentBackward' && value.charAt(start - 1) === '-') {
      removeIndex = digitCountBefore(value, start) - 1;
    } else if (event.inputType === 'deleteContentForward' && value.charAt(start) === '-') {
      removeIndex = digitCountBefore(value, start);
    }

    if (removeIndex == null || removeIndex < 0) {
      return false;
    }

    prevent(event);

    var digits = phoneDigits(value);
    var nextDigits = removeDigitAt(digits, removeIndex);
    var formatted = formatDigits(nextDigits);

    display.value = formatted;
    setHiddenValue(display, nextDigits);
    setCaret(display, caretAfterDigitCount(formatted, removeIndex, false));
    validate(display, false);

    return true;
  }

  function handleBeforeInput(event) {
    var display = event.target;

    if (!isDisplayInput(display)) {
      return;
    }

    if (handleBoundaryDelete(display, event)) {
      return;
    }

    if (event.inputType === 'insertText' && !/^\\d$/.test(event.data || '')) {
      prevent(event);
      return;
    }

    if (event.inputType === 'insertText') {
      var digits = phoneDigits(display.value);
      var selected = selectedDigitCount(display);
      var inserted = insertedDigitCount(event);

      if ((digits.length - selected + inserted) > 10) {
        prevent(event);
      }
    }
  }

  function handleInput(event) {
    var display = event.target;

    if (!isDisplayInput(display) || display.__phoneAuthSyncing) {
      return;
    }

    sync(display, { preserveCaret: true, showError: false });
  }

  function handlePaste(event) {
    var display = event.target;

    if (!isDisplayInput(display)) {
      return;
    }

    prevent(event);

    var clipboard = event.clipboardData || global.clipboardData;
    var text = clipboard && clipboard.getData ? clipboard.getData('text') : '';
    var digits = phoneDigits(text);
    var formatted = formatDigits(digits);

    display.dataset.touched = 'true';
    display.value = formatted;
    setHiddenValue(display, digits);
    setCaretToEnd(display);
    validate(display, true);
  }

  function handleBlur(event) {
    var display = event.target;

    if (!isDisplayInput(display)) {
      return;
    }

    display.dataset.touched = 'true';
    sync(display, { preserveCaret: false, showError: true });
  }

  function handleInvalid(event) {
    var display = event.target;

    if (!isDisplayInput(display)) {
      return;
    }

    display.dataset.touched = 'true';
    sync(display, { preserveCaret: false, showError: true });
  }

  function handleSubmit(event) {
    var form = event.target;

    if (!form || !form.querySelectorAll) {
      return;
    }

    var inputs = toArray(form.querySelectorAll('[data-phone-auth-phone-display]'));

    if (!inputs.length) {
      return;
    }

    var ok = true;

    inputs.forEach(function (display) {
      display.dataset.touched = 'true';
      sync(display, { preserveCaret: false, showError: true });

      if (!validate(display, true)) {
        ok = false;
      }
    });

    if (!ok) {
      prevent(event);

      if (inputs[0].reportValidity) {
        inputs[0].reportValidity();
      }

      if (inputs[0].focus) {
        inputs[0].focus();
      }
    }
  }

  function init(root) {
    displayInputs(root || global.document).forEach(function (display) {
      sync(display, { preserveCaret: false, showError: false });
    });
  }

  function displayInputs(root) {
    return toArray((root || global.document).querySelectorAll('[data-phone-auth-phone-display]'));
  }

  function install(doc) {
    doc = doc || global.document;

    if (!doc || installed) {
      return;
    }

    installed = true;

    doc.addEventListener('beforeinput', handleBeforeInput);
    doc.addEventListener('input', handleInput);
    doc.addEventListener('paste', handlePaste);
    doc.addEventListener('blur', handleBlur, true);
    doc.addEventListener('invalid', handleInvalid, true);
    doc.addEventListener('submit', handleSubmit, true);

    if (doc.readyState === 'loading') {
      doc.addEventListener('DOMContentLoaded', function () {
        init(doc);
      });
    } else {
      init(doc);
    }

    doc.addEventListener('htmx:afterSwap', function (event) {
      init(event.target || doc);
    });
  }

  var PhoneAuth = {
    rawDigits: rawDigits,
    phoneDigits: phoneDigits,
    formatDigits: formatDigits,
    formattedPhone: formattedPhone,
    validationMessage: validationMessage,
    validate: validate,
    sync: sync,
    handleBeforeInput: handleBeforeInput,
    handleInput: handleInput,
    handlePaste: handlePaste,
    handleBlur: handleBlur,
    handleInvalid: handleInvalid,
    handleSubmit: handleSubmit,
    install: install
  };

  global.HumanHelpPhoneAuth = PhoneAuth;

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = PhoneAuth;
  }

  if (global.document) {
    install(global.document);
  }
})(typeof window !== 'undefined' ? window : globalThis);
")

(defn phone-auth-script
  []
  [:script {:dangerouslySetInnerHTML
            {:__html phone-auth-js}}])
