/**
 * Custom Push2 instrument: Trichords
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 *
 * This code is intended as a tutorial, not for production usage.
 *
 * @module push
 * Interface to the Push2 controller.
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Constructor
 *
 * @param buttonPressed Callback invoked when a button is pressed or released
 */
exports.Push = function(buttonPressed) {
  // We cannot locate the Push until the Max device is fully initialized
  this.controller    = null;
  this.buttonPressed = buttonPressed;
};

/**
 * Class
 */
exports.Push.prototype = {
  /**
   * Locate the Push controller
   *
   * Should be called when the Max live is fully loaded; use 'live.thisdevice'.
   */
  init: function() {
    if(this.controller != null) return;
    this.controller = findPush();
    if(this.controller == null) return;

    var buttonPressed = this.buttonPressed;
    this.buttonMatrix = findButtonMatrix(this.controller, function(args) {
      // Careful: 'this' inside here is a different 'this'!
      if(args[0] == "value" && args.length == 5) {
        buttonPressed({"velocity": args[1], "col": args[2], "row": args[3]});
      }
    });
  }

  /**
   * Return if the Push2 controller was found
   */
, checkFound: function() {
    return (this.controller != null);
  }

  /**
   * Grab control of the button matrix
   */
, grab: function() {
    if(!this.checkFound()) return;
    this.controller.call("grab_control", "Button_Matrix");
  }

  /**
   * Release control of the button matrix
   */
, release: function() {
    if(!this.checkFound()) return;
    this.controller.call("release_control", "Button_Matrix");
  }
};

/*******************************************************************************
  Private functions
*******************************************************************************/

/**
 * Find the Push2 controller
 *
 * NOTE: We do not take into account that there might be more than one.
 */
function findPush() {
  var liveApp            = new LiveAPI(null, "live_app")
  var numControlSurfaces = liveApp.getcount("control_surfaces");

  for (var i = 0; i < numControlSurfaces; i++) {
    var controlSurface = new LiveAPI(null, "control_surfaces " + i);
    if (controlSurface.type == "Push2") {
      return controlSurface;
    }
  }

  return null;
}

/**
 * Find the button matrix on the Push2 controller
 */
function findButtonMatrix(push, callback) {
  var buttonMatrixId = push.call("get_control", "Button_Matrix");
  var buttonMatrix   = new LiveAPI(callback, buttonMatrixId);
  buttonMatrix.property = "value"; // Monitor the buttons
  return buttonMatrix;
}
