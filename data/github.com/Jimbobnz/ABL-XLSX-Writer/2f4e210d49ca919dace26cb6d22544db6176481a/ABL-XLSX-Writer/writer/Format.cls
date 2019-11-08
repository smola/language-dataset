
/*     Copyright (C) 2014 James Bowen                                                                                                                       */
/*                                                                                                                                                          */
/*     Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),   */
/*     to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,   */
/*     and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:           */
/*                                                                                                                                                          */
/*     The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.                       */
/*                                                                                                                                                          */
/*     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  */
/*     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER   */
/*     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS */
/*     IN THE SOFTWARE.                                                                                                                                     */

USING ABL-XLSX-Writer.ABL-XLSX-Writer.writer.*.
USING Progress.Json.ObjectModel.JsonObject.

CLASS ABL-XLSX-Writer.ABL-XLSX-Writer.writer.Format INHERITS xmlwriter:

    /** Font attributes **/
    DEFINE PUBLIC PROPERTY has_font            AS LOGICAL              NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY font_index          AS INTEGER              NO-UNDO GET. SET.  
    DEFINE PUBLIC PROPERTY bold                AS LOGICAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_colour         AS CHARACTER            NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_charset        AS INTEGER              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_family         AS INTEGER              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_outline        AS LOGICAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_script         AS INTEGER              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_shadow         AS LOGICAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_strike         AS LOGICAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_name           AS CHARACTER            NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY italic              AS LOGICAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY font_size           AS DECIMAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY underline           AS INTEGER              NO-UNDO GET. PRIVATE SET.
    
    DEFINE PUBLIC PROPERTY num_format          AS CHARACTER            NO-UNDO GET. PRIVATE SET.

    DEFINE PUBLIC PROPERTY locked              AS LOGICAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY hidden              AS LOGICAL              NO-UNDO GET. PRIVATE SET.  
    DEFINE PUBLIC PROPERTY shrink              AS LOGICAL              NO-UNDO GET. PRIVATE SET.  
    
    DEFINE PUBLIC PROPERTY just_distrib        AS LOGICAL              NO-UNDO GET. PRIVATE SET. 
    DEFINE PUBLIC PROPERTY text_wrap           AS LOGICAL              NO-UNDO GET. PRIVATE SET. 
    
    DEFINE PUBLIC PROPERTY center_across       AS LOGICAL              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY text_v_align        AS INTEGER              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY text_h_align        AS INTEGER              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY indent              AS INTEGER              NO-UNDO GET. PRIVATE SET.  
    DEFINE PUBLIC PROPERTY rotation            AS INTEGER              NO-UNDO GET. PRIVATE SET.
    DEFINE PUBLIC PROPERTY reading_order       AS INTEGER              NO-UNDO GET. PRIVATE SET.
    
    DEFINE PUBLIC PROPERTY pattern             AS INTEGER              NO-UNDO GET. SET.   
    DEFINE PUBLIC PROPERTY bg_colour           AS CHARACTER            NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY fg_colour           AS CHARACTER            NO-UNDO GET. SET.

    DEFINE PUBLIC PROPERTY num_format_index    AS INTEGER              NO-UNDO GET. SET.  
    DEFINE PUBLIC PROPERTY border_index        AS INTEGER              NO-UNDO GET. SET.  
    
    DEFINE PUBLIC PROPERTY fill_index          AS INTEGER              NO-UNDO GET. SET.  
    DEFINE PUBLIC PROPERTY has_fill            AS LOGICAL              NO-UNDO GET. SET.

    DEFINE PUBLIC PROPERTY xf_index             AS INTEGER INITIAL 0   NO-UNDO GET. SET.  

    DEFINE PRIVATE PROPERTY named_colours       AS CLASS JsonObject     NO-UNDO GET. SET.
    DEFINE PRIVATE PROPERTY xf_Format_indices   AS CLASS JsonObject     NO-UNDO GET. SET.
    DEFINE PRIVATE PROPERTY dxf_Format_indices  AS CLASS JsonObject     NO-UNDO GET. SET.
    
    DEFINE PRIVATE TEMP-TABLE tt_Alignment_Property NO-UNDO
        FIELD Align_Property AS CHARACTER
        FIELD Align_Value    AS CHARACTER.
    

    CONSTRUCTOR Format(INPUT in_xf_Format_indices  AS CLASS JsonObject,
                       INPUT in_dxf_Format_indices AS CLASS JsonObject):

        THIS-OBJECT:xf_Format_indices  = in_xf_Format_indices.
        THIS-OBJECT:dxf_Format_indices = in_dxf_Format_indices.
        
/*         THIS-OBJECT:xf_index = ?. */

        THIS-OBJECT:font_name       = 'Calibri':U.
        THIS-OBJECT:font_size       = 11.
        THIS-OBJECT:font_colour     = '#000000'.    /** Default to Black **/
        THIS-OBJECT:font_family     = 2.
        THIS-OBJECT:bold            = FALSE.        
        THIS-OBJECT:italic          = FALSE.        
        THIS-OBJECT:underline       = 0.            /** Values 0,1,2,3,4 **/
        THIS-OBJECT:font_outline    = FALSE.        
        THIS-OBJECT:font_shadow     = FALSE.
        THIS-OBJECT:font_strike     = FALSE.
        THIS-OBJECT:font_script     = 0.           /** Values 0,1 & 2**/ 

        THIS-OBJECT:text_h_align    = 0.    /** left **/
        THIS-OBJECT:text_v_align    = 0.    /** vertical center**/
        THIS-OBJECT:center_across   = FALSE.
        THIS-OBJECT:indent          = 0.
        THIS-OBJECT:rotation        = 0.
        THIS-OBJECT:reading_order   = 0.
        THIS-OBJECT:pattern         = 1.    /** 1 = None **/

        THIS-OBJECT:num_format_index = 0.
        THIS-OBJECT:num_format      = '0'.  /** See Excel for a list of number formats **/

        named_colours = NEW JsonObject().
        named_colours:add('black','#000000').
        named_colours:add('blue','#0000FF').
        named_colours:add('brown','#800000').
        named_colours:add('cyan','#00FFFF').
        named_colours:add('gray','#808080').
        named_colours:add('green','#008000').
        named_colours:add('lime','#00FF00').
        named_colours:add('magenta','#FF00FF').
        named_colours:add('navy','#000080').
        named_colours:add('orange','#FF6600').
        named_colours:add('pink','#FF00FF').
        named_colours:add('purple','#800080').
        named_colours:add('red','#FF0000').
        named_colours:add('silver','#C0C0C0').
        named_colours:add('white','#FFFFFF').
        named_colours:add('yellow','#FFFF00').

    END CONSTRUCTOR.

    METHOD PRIVATE CHARACTER _get_colour(INPUT pch_Colour AS CHARACTER):

        IF THIS-OBJECT:named_colours:Has(pch_Colour) THEN
            RETURN THIS-OBJECT:named_colours:GetCharacter(INPUT pch_Colour).
        ELSE
        DO:
            IF pch_Colour MATCHES '#......' THEN
                RETURN pch_Colour.
            ELSE
                RETURN ''.
        END.
            
    END METHOD.

    METHOD PUBLIC INTEGER _get_xf_index():

        DEFINE VARIABLE key           AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE PropertyNames AS CHARACTER EXTENT NO-UNDO.
        DEFINE VARIABLE in_Index      AS INTEGER          NO-UNDO.
                    
        IF LOGICAL(THIS-OBJECT:xf_index) THEN
            in_Index = THIS-OBJECT:xf_index.
        ELSE
        DO:
            key = THIS-OBJECT:_get_format_key().

            IF THIS-OBJECT:xf_format_indices:HAS(INPUT key) THEN
                in_Index = THIS-OBJECT:xf_format_indices:GetInteger(INPUT key).
            ELSE
            DO:
                
                PropertyNames = THIS-OBJECT:xf_format_indices:GetNames().
                
                IF EXTENT(PropertyNames) EQ ? THEN
                    in_Index = 1.
                ELSE                    
                    in_Index = 1 + EXTENT(PropertyNames).
                
                THIS-OBJECT:xf_format_indices:ADD(INPUT key, 
                                                  INPUT in_Index).

                THIS-OBJECT:xf_index = in_Index.
        
            END.
        END.

        RETURN in_Index.
        
    END METHOD.

    METHOD PRIVATE CHARACTER _join(INPUT pchString      AS CHARACTER,
                                   INPUT pdeKeyValue    AS DECIMAL):
        RETURN THIS-OBJECT:_join(INPUT pchString,
                                 INPUT STRING(pdeKeyValue)
                                 ) .
    END METHOD.

    METHOD PRIVATE CHARACTER _join(INPUT pchString      AS CHARACTER,
                                   INPUT plgKeyValue    AS LOGICAL):
        RETURN THIS-OBJECT:_join(INPUT pchString,
                                 INPUT STRING(plgKeyValue)
                                 ) .
    END METHOD.

    METHOD PRIVATE CHARACTER _join(INPUT pchString      AS CHARACTER,
                                   INPUT pinKeyValue    AS INTEGER):
        RETURN THIS-OBJECT:_join(INPUT pchString,
                                 INPUT STRING(pinKeyValue)
                                 ) .
    END METHOD.

    METHOD PRIVATE CHARACTER _join(INPUT pchString      AS CHARACTER,
                                   INPUT pchKeyValue    AS CHARACTER):
        RETURN pchString + pchKeyValue + ':'.
    END METHOD.

    METHOD PUBLIC CHARACTER _get_font_key():
        /* Returns a unique hash key for a font. Used by Workbook.*/
        DEFINE VARIABLE chKey AS CHARACTER   NO-UNDO.

        chKey = THIS-OBJECT:_join(":",   THIS-OBJECT:bold).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_colour).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_charset).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_family).       
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_outline).      
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_script).       
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_shadow).       
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_strike).    
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_name).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:italic).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:font_size).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:underline).

        RETURN chKey.

    END METHOD.

    METHOD PUBLIC CHARACTER _get_format_key():
        /* Returns a unique hash key for a font. Used by Workbook.*/
        DEFINE VARIABLE chKey AS CHARACTER   NO-UNDO.

        chKey = THIS-OBJECT:_join(":",   THIS-OBJECT:_get_font_key() ).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:_get_border_key()).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:_get_fill_key()).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:_get_alignment_key()).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:num_format).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:locked).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:hidden).

        RETURN chKey.

    END METHOD.

    METHOD PRIVATE CHARACTER _get_border_key():
        RETURN ''.
    END METHOD.

    METHOD PUBLIC CHARACTER _get_fill_key():

        DEFINE VARIABLE chKey AS CHARACTER   NO-UNDO.

        chKey = THIS-OBJECT:_join(":", THIS-OBJECT:pattern ).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:bg_colour ).
        chKey = THIS-OBJECT:_join(chKey, THIS-OBJECT:fg_colour ).

        RETURN chKey.
    END METHOD.

    METHOD PUBLIC CHARACTER _get_alignment_key():
        DEFINE VARIABLE chKey AS CHARACTER   NO-UNDO.

        chKey = THIS-OBJECT:_join(":",    THIS-OBJECT:text_h_align ).
        chKey = THIS-OBJECT:_join(chKey,  THIS-OBJECT:text_v_align ).
        chKey = THIS-OBJECT:_join(chKey,  THIS-OBJECT:indent ).
        chKey = THIS-OBJECT:_join(chKey,  THIS-OBJECT:rotation ).
        chKey = THIS-OBJECT:_join(chKey,  THIS-OBJECT:text_wrap ).
        chKey = THIS-OBJECT:_join(chKey,  THIS-OBJECT:shrink ).
        chKey = THIS-OBJECT:_join(chKey,  THIS-OBJECT:reading_order ).

        RETURN chKey.
    END METHOD.        
    
    METHOD PUBLIC HANDLE _get_align_properties():

        DEFINE VARIABLE hn_Align_Properties AS HANDLE      NO-UNDO.

        CREATE SAX-ATTRIBUTES hn_Align_Properties.

        
/*         # Check if any alignment options in the format have been changed. */
        IF NOT (LOGICAL(THIS-OBJECT:text_h_align)   OR 
                LOGICAL(THIS-OBJECT:text_v_align)   OR 
                LOGICAL(THIS-OBJECT:indent)         OR
                LOGICAL(THIS-OBJECT:rotation)       OR 
                THIS-OBJECT:text_wrap               OR 
                THIS-OBJECT:shrink                  OR 
                LOGICAL(THIS-OBJECT:reading_order)) THEN
            RETURN hn_Align_Properties.   /** Nothing has changed.. **/


/*      # Indent is only allowed for horizontal left, right and distributed. */
/*      # If it is defined for any other alignment or no alignment has               */
/*      # been set then default to left alignment.                                   */
        IF (LOGICAL(THIS-OBJECT:indent) AND 
           THIS-OBJECT:text_h_align NE 1 AND
           THIS-OBJECT:text_h_align NE 3 AND
           THIS-OBJECT:text_h_align NE 7 ) THEN
            THIS-OBJECT:text_h_align = 1.
        

/*         # Check for properties that are mutually exclusive. */
        IF THIS-OBJECT:text_wrap THEN
            THIS-OBJECT:shrink = FALSE.

        IF THIS-OBJECT:text_h_align EQ 4 OR
           THIS-OBJECT:text_h_align EQ 5 OR
           THIS-OBJECT:text_h_align EQ 7 THEN
            THIS-OBJECT:shrink = FALSE.

        IF THIS-OBJECT:text_h_align NE 7 OR
           LOGICAL(THIS-OBJECT:indent) THEN
            THIS-OBJECT:just_distrib = FALSE.

        IF LOGICAL(THIS-OBJECT:text_h_align) THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('horizontal',
                                                 ENTRY( THIS-OBJECT:text_h_align,'left,center,right,fill,justify,centerContinuous,distributed')
                                                 ).

        IF THIS-OBJECT:just_distrib THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('justifyLastLine','1').
        
/*      # Property 'vertical' => 'bottom' is a default. It sets applyAlignment */
/*      # without an alignment sub-element.                                     */
        IF LOGICAL(THIS-OBJECT:text_v_align) AND 
           THIS-OBJECT:text_v_align NE 3 THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('vertical', 
                                                 ENTRY( THIS-OBJECT:text_v_align,'top,center,bottom,justify,distributed') 
                                                 ).
        
            
        IF LOGICAL(THIS-OBJECT:indent) THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('indent', 
                                                 STRING( THIS-OBJECT:indent ) 
                                                 ).
        IF LOGICAL(THIS-OBJECT:rotation) THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('textRotation', 
                                                 STRING( THIS-OBJECT:rotation )
                                                 ).

        IF LOGICAL( THIS-OBJECT:text_wrap ) THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('wrapText', 
                                                 '1').
        
        IF LOGICAL( THIS-OBJECT:shrink ) THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('shrinkToFit', 
                                                 '1').


        IF LOGICAL( THIS-OBJECT:reading_order ) THEN
            hn_Align_Properties:INSERT-ATTRIBUTE('readingOrder', 
                                                 STRING( THIS-OBJECT:reading_order ) 
                                                 ).

        RETURN hn_Align_Properties.
    END METHOD.
        

    METHOD PUBLIC VOID set_font_name(INPUT pch_font_name AS CHARACTER):
        IF pch_font_name NE '' THEN
        DO:
            THIS-OBJECT:font_name   = pch_font_name.
            THIS-OBJECT:has_font    = TRUE.
        END.
    END METHOD.

    METHOD PUBLIC VOID set_font_size(INPUT pin_font_size AS DECIMAL):
        IF pin_font_size NE ? OR pin_font_size NE 0 THEN
        DO:
            THIS-OBJECT:font_size   = pin_font_size.
            THIS-OBJECT:has_font    = TRUE.
        END.
    END METHOD.

    /** Wrapper Me non-British spelling of colour**/
    METHOD PUBLIC VOID set_font_color(INPUT pch_font_colour AS CHARACTER):
        THIS-OBJECT:set_font_colour(INPUT pch_font_colour).
    END METHOD.
    
    METHOD PUBLIC VOID set_font_colour(INPUT pch_font_colour AS CHARACTER):
        
        IF THIS-OBJECT:named_colours:HAS(pch_font_colour) THEN
            pch_font_colour = named_colours:GetCharacter( pch_font_colour ).
        
        IF pch_font_colour BEGINS '#' THEN               
            THIS-OBJECT:font_colour = pch_font_colour.
    
    END METHOD.

    METHOD PUBLIC VOID set_bold():
        THIS-OBJECT:set_bold(INPUT TRUE).
    END METHOD.

    METHOD PUBLIC VOID set_bold(INPUT lg_state AS LOGICAL):
        THIS-OBJECT:bold = lg_state.
    END METHOD.

    METHOD PUBLIC VOID set_italic():
        THIS-OBJECT:set_italic(INPUT TRUE).
    END METHOD.

    METHOD PUBLIC VOID set_italic(INPUT lg_italic AS LOGICAL):
        THIS-OBJECT:italic = lg_italic.
    END METHOD.

    METHOD PUBLIC VOID set_underline():
        THIS-OBJECT:set_underline(INPUT 1). /* Default */
    END METHOD.

    METHOD PUBLIC VOID set_underline(INPUT in_Style_Type AS INTEGER):
        THIS-OBJECT:underline = in_Style_Type.
    END METHOD.

    METHOD PUBLIC VOID set_font_strikeout():
        THIS-OBJECT:set_font_strikeout(INPUT TRUE).
    END METHOD.

    METHOD PUBLIC VOID set_font_strikeout(INPUT lg_font_strike AS LOGICAL):
        THIS-OBJECT:font_strike = lg_font_strike.
    END METHOD.

    METHOD PUBLIC VOID set_font_script():
        THIS-OBJECT:set_font_script(INPUT 1). /* Default is 1, superscript. */
    END METHOD.

    METHOD PUBLIC VOID set_font_script(INPUT in_font_script AS INTEGER):

        IF in_font_script GE 1 AND in_font_script LE 2 THEN
            THIS-OBJECT:font_script = in_font_script.
    END METHOD.

    METHOD PUBLIC VOID set_num_format(INPUT pch_format_code AS CHARACTER):
        THIS-OBJECT:num_format = pch_format_code.
    END METHOD.
    
    METHOD PUBLIC VOID set_locked():
        THIS-OBJECT:set_locked(INPUT TRUE).
    END METHOD.

    METHOD PUBLIC VOID set_locked(INPUT lg_locked AS LOGICAL):
        THIS-OBJECT:locked = lg_locked.
    END METHOD.

    METHOD PUBLIC VOID set_hidden():
        THIS-OBJECT:set_hidden(INPUT TRUE).
    END METHOD.

    METHOD PUBLIC VOID set_hidden(INPUT lg_hidden AS LOGICAL):
        THIS-OBJECT:locked = lg_hidden.
    END METHOD.    


    METHOD PUBLIC VOID set_align(INPUT pch_alignment AS CHARACTER):
        
    /** Horizontal alignment **/
        CASE pch_alignment:
            WHEN 'left' THEN 
                THIS-OBJECT:set_text_h_align(1).
            WHEN 'centre' OR 
            WHEN 'center' THEN 
                THIS-OBJECT:set_text_h_align(2).
            WHEN 'right' THEN 
                THIS-OBJECT:set_text_h_align(3).
            WHEN 'fill' THEN 
                THIS-OBJECT:set_text_h_align(4).
            WHEN 'justify' THEN 
                THIS-OBJECT:set_text_h_align(5).
            WHEN 'center_across' THEN 
                THIS-OBJECT:set_text_h_align(6).
            WHEN 'distributed' OR
            WHEN 'justify_distributed' THEN 
            DO:
                THIS-OBJECT:set_text_h_align(7).
                THIS-OBJECT:just_distrib = TRUE.
            END.
        END CASE.

/*         IF  pch_alignment EQ 'justify_distributed' THEN */
/*             THIS-OBJECT:just_distrib = 1.               */

        /* duck**/

    /** Vertical  alignment **/
        CASE pch_alignment:
            WHEN 'top' THEN 
                THIS-OBJECT:set_text_v_align(1).
            WHEN 'vcenter' OR
            WHEN 'vcentre' THEN 
                THIS-OBJECT:set_text_v_align(2).
            WHEN 'bottom' THEN 
                THIS-OBJECT:set_text_v_align(3).
            WHEN 'vjustify' THEN 
                THIS-OBJECT:set_text_v_align(4).
            WHEN 'vdistributed' THEN 
                THIS-OBJECT:set_text_v_align(5).
        END CASE.

    END METHOD.

    METHOD PRIVATE VOID set_text_h_align(INPUT in_hoz_align AS INTEGER):
        THIS-OBJECT:text_h_align = in_hoz_align.
    END METHOD.

    METHOD PRIVATE VOID set_text_v_align(INPUT in_ver_align AS INTEGER):
        THIS-OBJECT:text_v_align = in_ver_align.
    END METHOD.

    METHOD PUBLIC VOID set_center_across():
        THIS-OBJECT:center_across = TRUE.
    END METHOD.

    METHOD PUBLIC VOID set_text_wrap():
        THIS-OBJECT:set_text_wrap(TRUE).
    END METHOD.

    METHOD PUBLIC VOID set_text_wrap(INPUT lg_text_wrap AS LOGICAL):
        THIS-OBJECT:text_wrap = lg_text_wrap.
    END METHOD.
   
    METHOD PUBLIC VOID set_rotation(INPUT pin_rotation AS INTEGER):
        
        IF pin_rotation EQ 270 THEN
            pin_rotation = 255.
        ELSE 
            IF (pin_rotation GE -90 OR 
                pin_rotation LE 90) THEN
            IF pin_rotation LT 0 THEN
                pin_rotation = (pin_rotation * -1) + 90.
            ELSE
                MESSAGE "Rotation rotation outside range: -90 <= angle <= 90".

        THIS-OBJECT:rotation = pin_rotation.
    END METHOD.

    METHOD PUBLIC VOID set_indent(INPUT pin_level AS INTEGER):
        THIS-OBJECT:indent = pin_level.
    END METHOD.
   
    METHOD PUBLIC VOID set_shrink():
        THIS-OBJECT:set_shrink(TRUE).
    END METHOD.

    METHOD PUBLIC VOID set_shrink(INPUT lg_shrink AS LOGICAL):
        THIS-OBJECT:shrink = lg_shrink.
    END METHOD.

    METHOD PUBLIC VOID set_pattern(INPUT pin_pattern AS INTEGER):

        IF pin_pattern GT 0 AND pin_pattern LE 18 THEN
            THIS-OBJECT:pattern = pin_pattern.
        ELSE 
            THIS-OBJECT:pattern = 0.

    END METHOD.

    METHOD PUBLIC VOID set_bg_colour(INPUT pch_colour AS CHARACTER):
        THIS-OBJECT:bg_colour = THIS-OBJECT:_get_colour(INPUT pch_colour).

    END METHOD.

    METHOD PUBLIC VOID set_fg_colour(INPUT pch_colour AS CHARACTER):
        THIS-OBJECT:fg_colour = THIS-OBJECT:_get_colour(INPUT pch_colour).
    END METHOD.

END CLASS.

