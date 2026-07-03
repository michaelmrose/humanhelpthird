(ns net.humanhelp.components.phone-auth.scripts)

(def phone-format-js
  "(function(){
  if (window.__humanHelpPhoneAuthInstalled) return;
  window.__humanHelpPhoneAuthInstalled = true;

  function rawDigits(value) {
    return String(value || '').replace(/\\D/g, '');
  }

  function phoneDigits(value) {
    var digits = rawDigits(value);

    if (digits.length > 10 && digits.charAt(0) === '1') {
      digits = digits.slice(1);
    }

    return digits.slice(0, 10);
  }

  function formatDigits(digits, inputType) {
    var deleting = inputType && inputType.indexOf('delete') === 0;

    if (digits.length === 0) {
      return '';
    }

    if (digits.length <= 2) {
      return digits;
    }

    if (digits.length === 3) {
      return deleting ? digits : digits + '-';
    }

    if (digits.length <= 5) {
      return digits.slice(0, 3) + '-' + digits.slice(3);
    }

    if (digits.length === 6) {
      return deleting
        ? digits.slice(0, 3) + '-' + digits.slice(3, 6)
        : digits.slice(0, 3) + '-' + digits.slice(3, 6) + '-';
    }

    return digits.slice(0, 3) + '-' + digits.slice(3, 6) + '-' + digits.slice(6);
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
        seen++;

        if (seen >= digitCount) {
          if ((digitCount === 3 || digitCount === 6) &&
              value.charAt(i + 1) === '-') {
            return i + 2;
          }

          return i + 1;
        }
      }
    }

    return value.length;
  }

  function errorElement(input) {
    var id = input.dataset.phoneAuthErrorId;
    return id ? document.getElementById(id) : null;
  }

  function message(input, key, fallback) {
    return input.dataset[key] || fallback;
  }

  function setError(input, text, show) {
    var err = errorElement(input);

    if (text) {
      input.setCustomValidity(text);
      input.setAttribute('aria-invalid', 'true');

      if (show && err) {
        err.textContent = text;
        err.classList.remove('hidden');
      }
    } else {
      input.setCustomValidity('');
      input.setAttribute('aria-invalid', 'false');

      if (err) {
        err.textContent = '';
        err.classList.add('hidden');
      }
    }
  }

  function validatePhone(input, show) {
    var digits = phoneDigits(input.value);
    var text = '';

    if (digits.length === 0) {
      text = message(input,
                     'phoneAuthRequiredMessage',
                     'Enter your phone number.');
    } else if (digits.length !== 10) {
      text = message(input,
                     'phoneAuthInvalidMessage',
                     'Enter your phone number.');
    }

    setError(input, text, show || input.dataset.touched === 'true');

    return text === '';
  }

  function formatPhone(input, inputType) {
    if (!input || input.__phoneAuthFormatting) {
      return;
    }

    var before = input.value || '';
    var caret = input.selectionStart == null ? before.length : input.selectionStart;
    var wasAtEnd = caret === before.length;
    var digitPosition = digitCountBefore(before, caret);
    var digits = phoneDigits(before);
    var after = formatDigits(digits, inputType);

    if (before !== after) {
      input.__phoneAuthFormatting = true;
      input.value = after;

      var nextCaret = caretAfterDigitCount(after, digitPosition, wasAtEnd);

      try {
        input.setSelectionRange(nextCaret, nextCaret);
      } catch (_) {}

      input.__phoneAuthFormatting = false;
    }

    return after;
  }

  function phoneInputFromEvent(event) {
    var el = event.target;

    if (!el || !el.matches) {
      return null;
    }

    if (!el.matches('[data-phone-auth-format=\"us\"]')) {
      return null;
    }

    return el;
  }

  function initInput(input) {
    formatPhone(input, null);
    validatePhone(input, false);
  }

  function initAll(root) {
    root = root || document;

    root.querySelectorAll('[data-phone-auth-format=\"us\"]').forEach(initInput);
  }

  document.addEventListener('input', function(event) {
    var input = phoneInputFromEvent(event);

    if (!input || input.__phoneAuthFormatting) {
      return;
    }

    formatPhone(input, event.inputType || null);
    validatePhone(input, false);
  });

  document.addEventListener('paste', function(event) {
    var input = phoneInputFromEvent(event);

    if (!input) {
      return;
    }

    setTimeout(function() {
      formatPhone(input, 'insertFromPaste');
      input.dataset.touched = 'true';
      validatePhone(input, true);
    }, 0);
  });

  document.addEventListener('blur', function(event) {
    var input = phoneInputFromEvent(event);

    if (!input) {
      return;
    }

    input.dataset.touched = 'true';
    formatPhone(input, null);
    validatePhone(input, true);
  }, true);

  document.addEventListener('invalid', function(event) {
    var input = phoneInputFromEvent(event);

    if (!input) {
      return;
    }

    input.dataset.touched = 'true';
    formatPhone(input, null);
    validatePhone(input, true);
  }, true);

  document.addEventListener('submit', function(event) {
    var form = event.target;

    if (!form || !form.querySelectorAll) {
      return;
    }

    var inputs = form.querySelectorAll('[data-phone-auth-format=\"us\"]');

    if (!inputs.length) {
      return;
    }

    var valid = true;

    inputs.forEach(function(input) {
      input.dataset.touched = 'true';
      formatPhone(input, null);

      if (!validatePhone(input, true)) {
        valid = false;
      }
    });

    if (!valid) {
      event.preventDefault();

      if (inputs[0].reportValidity) {
        inputs[0].reportValidity();
      }

      inputs[0].focus();
    }
  }, true);

  document.addEventListener('DOMContentLoaded', function() {
    initAll(document);
  });

  document.addEventListener('htmx:afterSwap', function(event) {
    initAll(event.target || document);
  });
})();")

(defn phone-format-script
  []
  [:script {:dangerouslySetInnerHTML
            {:__html phone-format-js}}])
