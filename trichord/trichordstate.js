/**
 * Custom Push2 instrument: Trichords
 * This code is intended as a tutorial, not for production usage.
 *
 * @module trichordstate
 * @description Store and process the state of the trichord
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020
 */

/*******************************************************************************
  Constants
*******************************************************************************/

/**
 * Supported scales
 *
 * @enum {number}
 */
exports.Scale = {
  MIYAKO_BUSHI: 0
, RITSU: 1
, MIN_YO: 2
, RYU_KYU: 3
, CUSTOM: 4
};

/**
 * Scale notes
 *
 * @enum {number}
 */
exports.Note = {
    C : 0
  , Cs: 1
  , D : 2
  , Ds: 3
  , E : 4
  , F : 5
  , Fs: 6
  , G : 7
  , Gs: 8
  , A : 9
  , As: 10
  , B : 11
  };

/*******************************************************************************
  Public API
*******************************************************************************/

 /**
  * Construct initial state
  *
  * @constructor
  */
exports.TrichordState = function() {
  /**
   * Currently selected scale.
   *
   * @member {module:trichordstate.Scale}
   */
  this.scale = exports.Scale.MIYAKO_BUSHI;

  /**
   * Currently selected root.
   *
   * @member {module:trichordstate.Note}
   */
  this.root = exports.Note.C;

  /**
   * Currently selected note for the first trichord.
   * Interpreted as a distance in semi-tones from the 1st scale degree.
   * Only used when using the <code>CUSTOM</code> scale.
   *
   * @member {number}
   */
  this.custom1 = 0;

  /**
   * Currently selected note for the second trichord.
   * Interpreted as a distance in semi-tones from the 4th scale degree.
   * Only used when using the <code>CUSTOM</code> scale.
   *
   * @member {number}
   */
  this.custom2 = 0;
}

exports.TrichordState.prototype = {
  /**
   * Get the note used in the first trichord.
   *
   * @returns {number} Offset from the 1st scale degree.
   */
  getTrichord1: function() {
    with (exports.Note) {
      with (exports.Scale) {
        switch(this.scale) {
          case MIYAKO_BUSHI: return 0;
          case RITSU:        return 1;
          case MIN_YO:       return 2;
          case RYU_KYU:      return 3;
          case CUSTOM:       return this.custom1;
        }
      }
    }
  }

  /**
   * Get the note used in the second trichord.
   *
   * @returns {number} Offset from the 4th scale degree.
   */
, getTrichord2: function() {
    with (exports.Note) {
      with (exports.Scale) {
        switch(this.scale) {
          case MIYAKO_BUSHI: return 0;
          case RITSU:        return 1;
          case MIN_YO:       return 2;
          case RYU_KYU:      return 3;
          case CUSTOM:       return this.custom2;
        }
      }
    }
  }

  /**
   * Get the current scale as an array of notes.
   *
   * Takes the root note into account, and includes the root note the next
   * octave up. For example, if the scale is the Miyako-Bushi, and the root is
   * D, we would return <code>[D, Ds, G, A, As, D + 12]</code>.
   *
   * @returns {Array} Notes in the scale.
   */
, getScale: function() {
    var scale = new Array(6);

    with (exports.Note) {
      scale[0] = C;
      scale[1] = Cs + this.getTrichord1();
      scale[2] = F;
      scale[3] = G;
      scale[4] = Gs + this.getTrichord2();
      scale[5] = C + 12;
    }

    for(var i = 0; i <= 5; i++) {
      scale[i] += this.root;
    }

    return scale;
  }

  /**
   * Post current state to the console.
   */
, postState: function() {
    post("TricordState", this.scale, this.root, this.custom1, this.custom2, "\n");
  }
}
