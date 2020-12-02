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
    post(buttonMatrix.info);
    post();
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

  // Set the color in column 0, row 5 to 10
  buttonMatrix.call("send_value", 0, 5, 10);
}

function release() {
  post("releasing..\n");
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
