/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * SPEECH BUBBLE MODULE - Header                                                           *
 * by Gunnar Harboe (Snarky), v0.8.0                                                       *
 *                                                                                         *
 * Copyright (c) 2017 Gunnar Harboe                                                        *
 *                                                                                         *
 * This module allows you to display conversation text in comic book-style speech bubbles. *
 * The appearance of the speech bubbles can be extensively customized.                     *
 *                                                                                         *
 * GitHub repository:                                                                      *
 * https://github.com/messengerbag/SpeechBubble                                            *
 *                                                                                         *
 * AGS Forum thread:                                                                       *
 * http://www.adventuregamestudio.co.uk/forums/index.php?topic=55542                       *
 *                                                                                         *
 * THIS MODULE IS UNFINISHED. SOME FEATURES ARE NOT YET IMPLEMENTED,                       *
 * AND OTHERS MAY CHANGE BEFORE THE FINAL RELEASE.                                         *
 *                                                                                         *
 * To use, you call Character.SayBubble(). For example:                                    *
 *                                                                                         *
 *     player.SayBubble("This line will be said in a speech bubble");                      *
 *                                                                                         *
 * You can also use Character.SayAtBubble() to position the bubble elsewhere on screen,    *
 * and Character.SayBackgroundBubble() for non-blocking speech.                            *
 * Character.ThinkBubble() IS NOT YET IMPLEMENTED.                                         *
 *                                                                                         *
 * To configure the appearance of the speech bubbles, you set the SpeechBubble properties. *
 * This is usually best done in GlobalScript game_start(). For example:                    *
 *                                                                                         *
 *   function game_start()                                                                 *
 *   {                                                                                     *
 *     SpeechBubble.BorderColor = Game.GetColorFromRGB(0,128,0);                           *
 *     SpeechBubble.BackgroundColor = Game.GetColorFromRGB(128,255,128);                   *
 *     SpeechBubble.BackgroundTransparency = 33;                                           *
 *     SpeechBubble.PaddingTop = 5;                                                        *
 *     SpeechBubble.PaddingBottom = 5;                                                     *
 *     SpeechBubble.PaddingLeft = 15;                                                      *
 *     SpeechBubble.PaddingRight = 15;                                                     *
 *     // Other code                                                                       *
 *   }                                                                                     *
 *                                                                                         *
 * See the declaration below for the full list and explanation of the properties.          *
 *                                                                                         *
 * The module relies on built-in AGS settings as far as possible. In particular, it uses   *
 * these settings to customize the appearance and behavior of the speech bubbles:          *
 *                                                                                         *
 * - Character.SpeechColor                                                                 *
 * - game.bgspeech_stay_on_display                                                         *
 * - Game.MinimumTextDisplayTimeMs                                                         *
 * - Game.SpeechFont                                                                       *
 * - Game.TextReadingSpeed                                                                 *
 * - Speech.DisplayPostTimeMs                                                              *
 * - Speech.SkipStyle                                                                      *
 * - Speech.VoiceMode                                                                      *
 *                                                                                         *
 * Note that to get text-based lip sync to work, you need to provide an invisible font,    *
 * and set the SpeechBubble.InvisibleFont property accordingly. You may download one here: *
 *                                                                                         *
 *   http://www.angelfire.com/pr/pgpf/if.html                                              *
 *                                                                                         *
 * Finally, the module (usually) calls Character.Say() to render speech animation and play *
 * voice clips. If you are already using some custom Say module (e.g. TotalLipSync), you   *
 * may want to call a custom Say() function instead. To do this, simply change the         *
 * function call in SB_sayImpl() at the top of SpeechBubble.asc.                           *
 *                                                                                         *
 * This code is offered under the MIT License                                              *
 * https://opensource.org/licenses/MIT                                                     *
 *                                                                                         *
 * It is also licensed under a Creative Commons Attribution 4.0 International License.     *
 * https://creativecommons.org/licenses/by/4.0/                                            *
 *                                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/// The shape of a text outline
enum TextOutlineStyle
{
  /// A circular, rounded outline
  eTextOutlineRounded = 0, 
  /// A square "block" outline
  eTextOutlineSquare
};

/// Speech bubble settings
managed struct SpeechBubble
{
  #region Static Properties
  /// The GUI that will be used to display blocking bubbles if no GUI argument is passed. Default: null (use overlays) 
  import static attribute GUI* DefaultGui;              // $AUTOCOMPLETESTATICONLY$
  /// The background color of speech bubbles (an AGS color number). Default: 15 (white) 
  import static attribute int BackgroundColor;          // $AUTOCOMPLETESTATICONLY$
  /// The percentage by which speech bubble backgrounds are tinted by the speech color (0-100). Default: 0
  import static attribute int BackgroundSpeechTint;          // $AUTOCOMPLETESTATICONLY$
  /// The transparency of the speech bubble backgrounds (0-100). Default: 0
  import static attribute int BackgroundTransparency;   // $AUTOCOMPLETESTATICONLY$
  /// The border color of speech bubbles (an AGS color number). Default: 0 (black)
  import static attribute int BorderColor;              // $AUTOCOMPLETESTATICONLY$
  /// The percentage by which speech bubble borders are tinted by the speech color (0-100). Default: 0
  import static attribute int BorderSpeechTint;          // $AUTOCOMPLETESTATICONLY$
  /// The transparency of the speech bubble borders (0-100). Default: 0
  import static attribute int BorderTransparency;       // $AUTOCOMPLETESTATICONLY$
  /// The transparency of speech bubble text (0-100). Default: 0
  import static attribute int TextTransparency;         // $AUTOCOMPLETESTATICONLY$
  
  /// The color of any outline applied to speech bubble text (an AGS color number). Default: 0 (black)
  import static attribute int TextOutlineColor;         // $AUTOCOMPLETESTATICONLY$
  /// The percentage by which text outlines are tinted by the speech color (0-100). Default: 0
  import static attribute int TextOutlineSpeechTint;         // $AUTOCOMPLETESTATICONLY$
  /// The width of the outline applied to speech bubble text. Default: 0 (none)
  import static attribute int TextOutlineWidth;         // $AUTOCOMPLETESTATICONLY$
  /// The style of the outline applied to speech bubble text. Default: eTextOutlineRounded
  import static attribute TextOutlineStyle TextOutlineStyle;         // $AUTOCOMPLETESTATICONLY$
  
  /// How wide a line of text can be before it wraps. Add left+right padding for total speech bubble width. If <= 0, use default AGS text wrapping. Default: 0
  import static attribute int MaxTextWidth;             // $AUTOCOMPLETESTATICONLY$
  /// Pixels between the text and the top of speech bubbles. Default: 10
  import static attribute int PaddingTop;               // $AUTOCOMPLETESTATICONLY$
  /// Pixels between the text and the bottom of speech bubbles. Default: 10
  import static attribute int PaddingBottom;            // $AUTOCOMPLETESTATICONLY$
  /// Pixels between the text and the left of speech bubbles. Default: 20
  import static attribute int PaddingLeft;              // $AUTOCOMPLETESTATICONLY$
  /// Pixels between the text and the right of speech bubbles. Default: 20
  import static attribute int PaddingRight;             // $AUTOCOMPLETESTATICONLY$
  /// Pixels between the top of the character sprite and the bottom of the speech bubble tail (can be negative)
  import static attribute int HeightOverHead;           // $AUTOCOMPLETESTATICONLY$
  /// How many pixels to round the corners of the speech bubble by. Default: 8
  import static attribute int CornerRoundingRadius;     // $AUTOCOMPLETESTATICONLY$
  
  /// The speech bubble "tail" to use for talk bubbles ("Say" functions), as a String "pixel array". Must be null-terminated!
  import static attribute String TalkTail[];            // $AUTOCOMPLETESTATICONLY$
  /// Get the width of the speech bubble tail for talk bubbles
  import readonly static attribute int TalkTailWidth;   // $AUTOCOMPLETESTATICONLY$
  /// Get the height of the speech bubble tail for talk bubbles
  import readonly static attribute int TalkTailHeight;  // $AUTOCOMPLETESTATICONLY$
  /// The speech bubble "tail" to use for thought bubbles ("Think" function), as a String "pixel array". Must be null-terminated!
  import static attribute String ThinkTail[];           // $AUTOCOMPLETESTATICONLY$
  /// Get the width of the speech bubble tail for think bubbles
  import readonly static attribute int ThinkTailWidth;  // $AUTOCOMPLETESTATICONLY$
  /// Get the height of the speech bubble tail for think bubbles
  import readonly static attribute int ThinkTailHeight; // $AUTOCOMPLETESTATICONLY$
  
  /// The text alignment in speech bubbles. Default: eAlignCentre
  import static attribute Alignment TextAlign;          // $AUTOCOMPLETESTATICONLY$
  /// Set a font where all characters are invisible, to improve integration. Default: -1 (none) 
  import static attribute FontType InvisibleFont;       // $AUTOCOMPLETESTATICONLY$
  #endregion
  
  #region Instance Properties
  /// Get the Character that this speech bubble belongs to
  import readonly attribute Character* OwningCharacter;
  /// Get whether this speech bubble is valid (not removed from screen)
  import readonly attribute bool Valid;
  /// Get whether this speech bubble is displaying non-blocking background speech
  import readonly attribute bool IsBackgroundSpeech;
  /// Get whether this is a thought bubble
  import readonly attribute bool IsThinking;
  /// Get whether this speech bubble is displayed on a GUI
  import readonly attribute bool UsesGUI;
  /// Get whether the character is being (manually) animated
  import readonly attribute bool Animating;
  /// Get the text of this speech bubble
  import readonly attribute String Text;
  /// Get the rendered version of this speech bubble, as a DynamicSprite
  import readonly attribute DynamicSprite* BubbleSprite;
  /// Get the Overlay this speech bubble is rendered on (null if none)
  import readonly attribute Overlay* BubbleOverlay;
  /// Get the GUI this speech bubble is rendered on (null if none)
  import readonly attribute GUI* BubbleGUI;
  /// Get the total number of game loops this speech bubble is displayed before it times out (-1 if no timeout)
  import readonly attribute int TotalDuration;
  /// Get how many game loops this speech bubble has been displayed
  import readonly attribute int ElapsedDuration;
  /// Get/set the X screen-coordinate of this speech bubble's top-left corner
  import attribute int X;
  /// Get/set the Y screen-coordinate of this speech bubble's top-left corner
  import attribute int Y;
  #endregion
  
  #region Protected member variables
  // The underlying variables for the instance properties
  protected int _id;
  protected bool _valid;
  protected bool _isBackgroundSpeech;
  protected bool _isThinking;
  protected bool _isAnimating;
  protected bool _usesGui;
  protected int _totalDuration;
  protected int _elapsedDuration;
  protected int _x;
  protected int _y;
  #endregion
};

#region Character Extender functions
/// Like Character.Say(), but using a speech bubble.
import void SayBubble(this Character*, String message, GUI* bubbleGui=0);
/// Like SayBubble(), but the bubble will be positioned with the top-left corner at the given coordinates
import void SayAtBubble(this Character*, int x, int y, String message, GUI* bubbleGui=0);
/// Non-blocking speech, similar to SayBackground() - if animate is true, will play the speech animation
import SpeechBubble* SayBackgroundBubble(this Character*, String message, bool animate=true, GUI* bubbleGui=0);
/// Like Character.Think(), but using this module's thought bubble
import void ThinkBubble(this Character*, String message, GUI* bubbleGui=0);

/// The current height of the character (pixels from the Character.x position to the top of their sprite)
import int GetHeight(this Character*);
/// Interrupt the character if they are speaking in the background (returns whether they were)
import bool StopBackgroundBubble(this Character*);
/// Whether the character is speaking in a bubble
import bool IsSpeakingBubble(this Character*, bool includeBackground=true);
/// The speech bubble used by the character (null if none)
import SpeechBubble* GetSpeechBubble(this Character*);
#endregion
