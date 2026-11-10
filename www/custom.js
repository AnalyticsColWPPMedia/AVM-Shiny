/* =======================================================*/
/* 1. Download Button Interaction                         */
/* =======================================================*/

$(document).ready(function() {
  // Listener for clicks on custom download buttons
  // Adds a visual 'loading' state for 3 seconds to give immediate user feedback
  $(document).on('click', '.custom-download-btn', function() {
    var buttonId = $(this).attr('id');
    $(this).addClass('btn-downloading');

    // Remove the class after a timeout (simulating download completion)
    setTimeout(function() {
      $('#' + buttonId).removeClass('btn-downloading');
    }, 3000);
  });
});

/* =======================================================*/
/* 2. Shiny Custom Message Handlers                       */
/* =======================================================*/

// Listener for custom messages sent from the R server
// Allows R to programmatically toggle the 'btn-downloading' class on specific elements
Shiny.addCustomMessageHandler('toggleDownloadButtonClass', function(message) {
  var button = $('#' + message.id);

  if (message.downloading) {
    button.addClass('btn-downloading');
  } else {
    button.removeClass('btn-downloading');
  }
});

/* =======================================================*/
/* 3. Datepicker Z-Index & Positioning Fix                */
/* =======================================================*/

// IIFE (Immediately Invoked Function Expression) to handle Datepicker positioning issues.
// This solves the common problem where Bootstrap datepickers are hidden behind
// other UI elements due to 'overflow: hidden' or stacking contexts.
// It applies to every Shiny dateInput / dateRangeInput in the app (including
// inputs inside modules, whose ids are namespaced, e.g. 'avm_module_1-reporting_window').
(function() {

  // Any text input inside a Shiny date or date range input
  var DATE_INPUT_SELECTOR = '.shiny-date-input input, .shiny-date-range-input input';

  // Helper function to detach the datepicker DOM element and append it directly to the body
  function moveDatepickerToBody($input) {
    // Select only the dropdown currently open (a date range creates one per input)
    var $dp = $('.datepicker.datepicker-dropdown:visible');
    if (!$dp.length) return;

    // Detach from current parent and append to body to escape local stacking contexts
    if (!$dp.parent().is('body')) {
      $dp.detach().appendTo('body');
    }

    // Force high z-index and absolute positioning to ensure visibility
    $dp.css({
      'z-index': 999999,
      'position': 'absolute'
    });

    // Calculate position relative to the input element on the screen
    var off = $input.offset();
    if (!off) return;

    var h = $input.outerHeight() || 0;

    // Position the dropdown just below the input box
    $dp.css({
      top: (off.top + h + 6) + 'px',
      left: off.left + 'px'
    });
  }

  // Event delegation for focus/click on any date input (works for inputs
  // rendered later, e.g. inside conditionalPanel or renderUI)
  $(document).on('focus click', DATE_INPUT_SELECTOR, function() {
    var $inp = $(this);
    // Slight delay to ensure the Datepicker DOM element has been created by the library
    setTimeout(function() {
      moveDatepickerToBody($inp);
    }, 0);
  });

  // Reposition datepicker dynamically when window scrolls or resizes
  // ensures the popup stays attached to the input field visually
  $(window).on('scroll resize', function() {
    var $active = $(DATE_INPUT_SELECTOR).filter(':focus');
    if ($active.length) {
      moveDatepickerToBody($active.first());
    }
  });
})();
