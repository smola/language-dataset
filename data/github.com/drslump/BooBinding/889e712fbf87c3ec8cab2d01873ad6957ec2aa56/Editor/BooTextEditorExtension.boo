namespace BooBinding.Editor

import System
import Gdk from "gdk-sharp" as Gdk
import Mono.TextEditor #(TextEditorData)
import MonoDevelop.Ide.Gui.Content  #(CompletionTextEditorExtension)
import MonoDevelop.Ide.CodeCompletion #(CodeCompletionContext)


class BooTextEditorExtension(CompletionTextEditorExtension):
    # TODO: The following methods should be enough to expose autocompletion
    override def GetCurrentParameterIndex(startOffset as int) as int:
        return -1

    #override def GetParameterCompletionCommandOffset([out] cpos as int) as bool:
    #    cpos = 0
    #    return false

    override def HandleCodeCompletion(ctx as CodeCompletionContext, chr as char, ref wordlen as int) as ICompletionDataList:
        print 'HandleCodeCompletion'
        return null

    override def HandleParameterCompletion(ctx as CodeCompletionContext, chr as char) as ParameterDataProvider:
        return null


    override def KeyPress(key as Gdk.Key, keyChar as char, modifier as Gdk.ModifierType) as bool:
    """ This should handle basic formatting when pressing enter.
        NOTE: In MD4 we should probably use the formatting API but this is simpler
    """
        if key == Gdk.Key.Return and Editor.Options.IndentStyle == IndentStyle.Smart | IndentStyle.Virtual:
            line = Editor.Caret.Line
            text = Editor.GetLineText(line).TrimEnd()
            if text.EndsWith(':'):
                Editor.Insert(
                    Editor.Caret.Offset,
                    "\n" + Editor.Options.IndentationString + GetIndent(Editor, line)
                )
                return false

        # Forward to other extensions
        if Next == null:
            return true
        else:
            return Next.KeyPress(key, keyChar, modifier)

    private def GetIndent(d as TextEditorData, line as int):
        # TODO: does TextEditorData already have methods to get the indent?
        text = d.GetLineText(line)
        whitespaces = System.Text.StringBuilder()
        for ch in text:
            if not char.IsWhiteSpace(ch):
                break
            whitespaces.Append(ch)
        return whitespaces.ToString()    


/*
class BooFormatter(AbstractAdvancedFormatter):
    static internal readonly string MimeType = "text/x-boo"

    overrride SupportsOnTheFlyFormatting:
        get: return false

    override SupportsCorrectingIndent:
        get: return true

    public override def CorrectIndenting(policyParent as PolicyContainer, mimeTypeChain as string*, data as TextEditorData, line as int):
        lineSegment = data.Document.GetLine(line)
        return unless lineSegment

        policy = policyParent.Get[of CSharpFormattingPolicy](mimeTypeChain)
        textPolicy = policyParent.Get[of TextStylePolicy](mimeTypeChain)
        tracker = DocumentStateTracker[of CSharpIndentEngine](CSharpIndentEngine(policy, textPolicy), data)
        tracker.UpdateEngine(lineSegment.Offset)
        for i in range(lineSegment.Offset, lineSegment.Offset + lineSegment.Length):
            tracker.Engine.Push(data.Document.GetCharAt(i))

        curIndent = lineSegment.GetIndentation(data.Document)

        int nlwsp = curIndent.Length;
        if (!tracker.Engine.LineBeganInsideMultiLineComment || (nlwsp < lineSegment.LengthIncludingDelimiter && data.Document.GetCharAt (lineSegment.Offset + nlwsp) == '*')) {
            // Possibly replace the indent
            string newIndent = tracker.Engine.ThisLineIndent;
            if (newIndent != curIndent) 
                data.Replace (lineSegment.Offset, nlwsp, newIndent);
        }
        tracker.Dispose ();

class TextEditorExtension(CompletionTextEditorExtension):

    public override def KeyPress(key as Gdk.Key, keyChar as char, modifier as Gdk.ModifierType) as bool:
        line as int
        column as int
        Editor.GetLineColumnFromPosition(Editor.CursorPosition, line, column)
        lineText = Editor.GetLineText(line)

        // smart formatting strategy
        if TextEditorProperties.IndentStyle == IndentStyle.Smart:
            if key == Gdk.Key.Return:
                if lineText.TrimEnd().EndsWith(":"):
                    Editor.InsertText(
                        Editor.CursorPosition, 
                        "\n" + TextEditorProperties.IndentString + GetIndent(Editor, line)
                    )
                    return false;

        return base.KeyPress (key, keyChar, modifier)

    private def GetIndent(d as TextEditor, line as int):
        text = d.GetLineText(line)
        whitespaces = StringBuilder()
        for ch in text:
            if not char.IsWhiteSpace(ch):
                break
            whitespaces.Append(ch)
        return whitespaces.ToString()
*/