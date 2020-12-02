/*
 * Push integration within Ableton Live
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 */

inlets  = 1;
outputs = 1;

/**
 * Reference to the Push
 *
 * Initialised in 'find_push'.
 */
var push = null;

/**
 * Reference to the button matrix of the Push
 *
 * Initialised in 'find_push'.
 */
var buttonMatrix = null;

function bang() {
  post("Banging away!!\n");
}

/**
 * Task for the background animation
 *
 * TODO: We should synchronize this to the clock.
 */
var bgAnimTask = new Task(updateBgAnim, this);
bgAnimTask.interval = 250;

/**
 * Find the Push2 controller
 *
 * Should be called when the Max device is fully loaded; use
 * 'live.thisdevice' to trigger this.
 */
function find_push() {
  var app   = new LiveAPI(null, "live_app")
  var numCS = app.getcount("control_surfaces");

  for (var i = 0; i < numCS; i++) {
    var cs = new LiveAPI(null, "control_surfaces " + i);
    if(cs.type == "Push2") {
      push = cs;
    }
  }

  if(push == null) {
    post("No push found :(\n");
    return;
  } else {
    post("Found the push!\n");
  }

  buttonMatrix = new LiveAPI(live_api_callback, push.call("get_control", "Button_Matrix"));
  if(buttonMatrix == null) {
    post("Button matrix not found :(\n");
  } else {
    post("Found the button matrix!\n");
    // Invoke the callback every time the value changes
    // (allows us to see when buttons are pressed)
    buttonMatrix.property = "value";
    //post(buttonMatrix.info);
    //post();
  }

/*
    post("** BEGIN INFO **\n");
    post(push.info);
    post();
    post("** END INFO **\n");

    var controlNames = push.call("get_control_names");
    post(Array.isArray(controlNames));
    var j = 0;
    for(var i = 0; i < controlNames.length; i++) {
      if(controlNames[i] == "control") {
        ++i;
        var name    = controlNames[i];
        var control = new LiveAPI(null, push.call("get_control", name));
        post(j + ": " + name + ", " + control.get("name") + "\n");
        j++;
      }
    }
*/
}

function grab() {
  post("grabbing..\n");
  // Using just the name works fine as well
  //  push.call("grab_control", "id", buttonMatrix.id);
  push.call("grab_control", "Button_Matrix");

  startBgAnim();
}

function release() {
  post("releasing..\n");
  stopBgAnim();
  push.call("release_control", "id", buttonMatrix.id);
}

/**
 * Initialize the patcher
 *
 * Use the 'live.thisdevice' object to determine when your Max Device has
 * completely loaded; the object sends a 'bang' from its left outlet when the
 * device is fully initialized (including the Live API). The 'init' message
 * should be sent in response to that 'bang'.
 */
function init() {
  if(api == null) {
    post("Initialising..\n");
    api = new LiveAPI(live_api_callback);
    post("Initialisation done.\n");
  }
}

function live_api_callback(args) {
  post("callback called with arguments:", args, "\n")
}

/*******************************************************************************
  Background animation
*******************************************************************************/

/**
 * The animation data
 */
var bgAnim = [
    [ [2, 2, 11], [5, 2, 11], [5, 5, 11], [2, 5, 11] ]
  , [ [3, 2, 10] ]
  , [ [4, 2, 10] ]
  , [ [2, 2, 12], [5, 2, 12], [5, 5, 12], [2, 5, 12] ]
  , [ [5, 3, 10] ]
  , [ [5, 4, 10] ]
  , [ [2, 2, 13], [5, 2, 13], [5, 5, 13], [2, 5, 13] ]
  , [ [4, 5, 10] ]
  , [ [3, 5, 10] ]
  , [ [2, 2, 14], [5, 2, 14], [5, 5, 14], [2, 5, 14] ]
  , [ [2, 4, 10] ]
  , [ [2, 3, 10] ]
];

/**
 * Current index into the animation
 */
var bgAnimIndex = null;

/**
 * Current state of the buttons (8x8 array containing button colors)
 */
var bgAnimCurrent = null;

/**
 * Start the animation
 */
function startBgAnim() {
  bgAnimIndex   = 0;
  bgAnimCurrent = mkEmptyFrame();
  bgAnimTask.repeat();
}
startBgAnim.local = 1;

/**
 * Stop the animation
 */
function stopBgAnim() {
  bgAnimTask.cancel();
}
stopBgAnim.local = 1;

/**
 * Create an empty frame
 *
 * Returns a 8x8 2D matrix.
 */
function mkEmptyFrame() {
  var frame = new Array();
  for(var i = 0; i < 8; i++) {
    var col = new Array();
    frame[i] = col;

    for(var j = 0; j < 8; j++) {
      col[j] = 0;
    }
  }

  return frame;
}
mkEmptyFrame.local = 1;

/**
 * Construct a frame from a series of instructions
 *
 * Each instruction should be a a list of 3 elements: row, column, color.
 */
function constructFrame(instructions) {
  var frame = mkEmptyFrame();

  for(var i in instructions) {
    var instruction = instructions[i];
    frame[instruction[0]][instruction[1]] = instruction[2];
  }

  return frame;
}
constructFrame.local = 1;

/**
 * Update the state of the push
 */
function updateFrame(newFrame) {
  for(var i = 0; i < 8; i++) {
    for(var j = 0; j < 8; j++) {
      var oldColor = bgAnimCurrent[i][j];
      var newColor = newFrame[i][j];
      if(oldColor != newColor) {
        buttonMatrix.call("send_value", i, j, newColor);
        bgAnimCurrent[i][j] = newColor;
      }
    }
  }
}
updateFrame.local = 1;

/**
 * Call-back used to update the animation
 */
function updateBgAnim(args) {
  updateFrame(constructFrame(bgAnim[bgAnimIndex]));
  bgAnimIndex = (bgAnimIndex + 1) % bgAnim.length;
}
updateBgAnim.local = 1;
