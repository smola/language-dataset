namespace Boo.Hints

import System.Reflection(BindingFlags)
import System.Collections.Generic(Dictionary)
import System.Diagnostics(Trace)
import Boo.Lang.Compiler(Ast)
import Boo.Lang.Environments(my)
import Boo.Lang.Compiler.TypeSystem
import Boo.Lang.Compiler.TypeSystem.Services(NameResolutionService, TypeCompatibilityRules)
import Boo.Lang.PatternMatching
import Boo.Hints.Messages as Messages
import Boo.Hints.Visitors as Visitors


class Commands:

    [Getter(Index)]
    _index as ProjectIndex

    def constructor(index as ProjectIndex):
        _index = index

    virtual def parse(query as Messages.Query) as Messages.Parse:
    """ Parse the given code reporting back any errors and warnings issued by
        the compiler.

        If extra information is requested the parse is performed with a type
        resolving compiler pipeline which will detect more errors than invalid
        syntax.
    """
        msg = Messages.Parse()

        fn = Index.WithParser
        if query.extra:
            fn = Index.WithCompiler

        fn(query.fname, query.code) do (module):
            for warn in Index.Context.Warnings:
                msg.warnings.Add(
                    Messages.Parse.Error(code: warn.Code, message: warn.Message, line: warn.LexicalInfo.Line, column: warn.LexicalInfo.Column)
                )
            for error in Index.Context.Errors:
                msg.errors.Add(
                    Messages.Parse.Error(code: error.Code, message: error.Message, line: error.LexicalInfo.Line, column: error.LexicalInfo.Column)
                )

        return msg

    virtual def outline(query as Messages.Query) as Messages.Node:
    """ Obtain an outline tree for the source code
    """
        root = Messages.Node()
        Index.WithParser(query.fname, query.code) do (module):
            module.Accept(Visitors.Outline(root))

        return root

    virtual def namespaces(query as Messages.Query) as Messages.Hints:
    """ Obtain the list of top level namespaces available in the compiler
    """
        msg = Messages.Hints()
        msg.scope = 'namespaces'
        # Run inside a parse to make sure we have the environment properly setup
        Index.WithParser('namespaces.cmd', '') do (module):
            nrs = my(NameResolutionService)
            for member in nrs.GlobalNamespace.GetMembers():
                continue if member.EntityType != EntityType.Namespace
                ProcessEntity(msg, member, query.extra)

        return msg

    virtual def builtins(query as Messages.Query) as Messages.Hints:
    """ Obtain the list of builtins available in the current compiler
    """
        msg = Messages.Hints()
        msg.scope = 'builtins'
        # Run inside a parse to make sure we have the environment properly setup
        Index.WithParser('builtins.cmd', '') do (module):
            tss = my(TypeSystemServices)

            # HACK: Uses reflection to obtain the map of primitives since it's private
            fi = tss.GetType().GetField('_primitives', BindingFlags.NonPublic | BindingFlags.Instance)
            prims as Dictionary[of string, IEntity] = fi.GetValue(tss)
            for prim in prims:
                ProcessEntity(msg, prim.Value, prim.Key, query.extra)

            # Get builtins
            for member in Index.StaticMembersOf(tss.BuiltinsType):
                ProcessEntity(msg, member, query.extra)

        return msg

    virtual def entity(query as Messages.Query) as Messages.Hints:
    """ Get information about a given entity based on the line and column of its
        first letter.
        If an additional param with true is given then we query based on the entity
        full name, in order to obtain all possible candidates.
    """
        msg = Messages.Hints()
        msg.scope = 'entity'

        Index.WithCompiler(query.fname, query.code) do (module):
            ent = Visitors.LineFinder(query.line, query.column).FindIn(module)
            if ent:
                if query.GetBoolParam(0):
                    # It seems like Boo only provides ambiguous information for internal
                    # entities. Here we resolve again based on the entity full name to get
                    # all possible candidates.
                    nrs = my(NameResolutionService)
                    all = nrs.ResolveQualifiedName(ent.FullName)
                    if all:
                        ProcessEntity(msg, all, query.extra)
                        return

                ProcessEntity(msg, ent, query.extra)

        return msg

    virtual def locals(query as Messages.Query) as Messages.Hints:
    """ Obtain hints for local symbols available in a method. This includes
        parameter definitions, method variables, closure variables and loop
        variables.
    """
        msg = Messages.Hints()
        msg.scope = 'locals'

        Index.WithCompiler(query.fname, query.code) do (module):
            for entity in Visitors.LocalsFinder(query.fname, query.line).FindIn(module):
                ProcessEntity(msg, entity, query.extra)

        return msg

    protected def GetUnbalancedParens(code as string):
    """ Given a piece of code find out if it ends with unbalanced parens or brackets.
        The algorithm is approximate, it may not always give correct results in some 
        complicated code but it should hopefully help to fix most invalid syntax before
        feeding the code to the compiler. Since this method will be used for auto
        completion, it will usually be called while the user is at the middle of writing
        a correct statement but not fully valid at that point.
    """
        def consume_comment(code as string, ofs as int):
            orig = ofs
            while ofs and code[ofs - 1] != char('\n'):
                ofs -= 1
                if code[ofs] == char('#'):
                    return ofs
            return orig

        def consume_quoted(code as string, ofs as int, ch as char):
            while ofs and code[ofs - 1] != ch: 
                ofs -= 1
            return (ofs - 1 if ofs > 0 else 0)

        mapping = {
            char('('): char(')'),
            char('{'): char('}'),
            char('['): char(']'),
        }

        closed = []
        unbalanced = []

        ofs = len(code)
        while ofs:
            ofs -= 1
            ch = code[ofs]

            if ofs and ch == char('\n'):
                ofs = consume_comment(code, ofs)

                # Consume white space
                while ofs and char.IsWhiteSpace(code[ofs - 1]):
                    ofs -= 1
                ch = code[ofs]

                if not char.IsPunctuation(ch) and ch not in (char('+'),):
                    break

            if ch in (char(')'), char('}'), char(']')):
                closed.Add(ch)
            elif ch in (char('('), char('{'), char('[')):
                if len(closed) and closed[-1] == mapping[ch]:
                    closed.Pop()
                else:
                    unbalanced.Add(mapping[ch])
            elif ch in (char('"'), "'"[0], char('`')):
                ofs = consume_quoted(code, ofs, ch)

        return unbalanced

    protected def GetImportNamespace(code as string) as string:
    """ If the code ends with an import statement returns the target namespace.
    """
        # Handle explicit symbol imports with the form `import System(IO, Diagnost`
        if code.LastIndexOf('(') > code.LastIndexOf(')'):
            ofs = code.LastIndexOf('(')
        else:
            ofs = len(code)

        # Get the line where the offset is
        ofs = code.LastIndexOf('\n', ofs) + 1
        line = code.Substring(ofs)

        m = /^(import|from)\s+([\w\.]+)?/.Match(line)
        if not m.Success:
            return null
        return m.Groups[2].Value.TrimEnd(char('.'))

    protected def FilterPrefix(msg as Messages.Hints, prefix as string):
        if prefix:
            msg.hints.RemoveAll({ h | not h.name.StartsWith(prefix) })
        return msg

    virtual def complete(query as Messages.Query) as Messages.Hints:
    """ Obtain hints for completing an expression. If the reported offset is not just 
        after a dot it will use any ident before it as a prefix. If still a dot is not
        found then it will query all symbols available at that point, this includes 
        local and global symbols as well as members of the enclosing type.

        When the first argument is true it will skip querying for global symbols, this 
        is useful for editor plugins that cache globals on their own for performance 
        reasons.
    """
        msg = Messages.Hints()
        msg.scope = 'complete'

        ofs = query.offset
        skip_globals = query.GetBoolParam(0)
        skip_extensions = query.GetBoolParam(1)

        # Consume the previous word if there is one and use it to filter results.
        prefix = ''
        while ofs and char.IsLetterOrDigit(query.code[ofs - 1]):
            prefix = query.code[ofs - 1] + prefix
            ofs--

        left = query.code.Substring(0, ofs)
        right = query.code.Substring(ofs)

        # Extract the line where the offset is
        line = left.Substring(left.LastIndexOf(char('\n')) + 1)

        # We may be inside an import statement but defining an alias
        if not line.EndsWith(' as '):
            # Check if it's an import statement
            ns = GetImportNamespace(left)
            if ns is not null:
                # Query top-level namespaces
                if ns == '': 
                    msg = namespaces(query)
                    msg.scope = 'import'
                    return FilterPrefix(msg, prefix)
                else:
                    # Perform the query against the namespace
                    query.code = ns + '.'
                    query.offset = len(query.code)
                    msg = complete(query)
                    msg.scope = 'import'
                    # TODO: What can we actually import in Boo?
                    msg.hints.RemoveAll({ h | h.node not in ('Ambiguous', 'Namespace', 'Type', 'Macro') })
                    return FilterPrefix(msg, prefix)

        # Detect if we are interested only on members
        if line.EndsWith('.'):
            msg.scope = 'members'
        else:
            # In some contexts we are only interested on types
            if line =~ /(\sas|\sof|\[of)\s+\(?$/ or line =~ /\b(class|struct|interface)\s[^\(]+\(/:
                msg.scope = 'type'
                if not skip_globals:
                    msg.hints += builtins(query).hints
                    msg.hints += namespaces(query).hints

            # When naming stuff we don't want completions
            elif line =~ /\b(class|struct|interface|enum|macro|def)\s+$/:
                msg.scope = 'name'
                return msg
            # Parameter definitions don't need completions (note that types are handled above)
            elif line =~ /(def\s\w+|def|do)\s*\([^)]*$/:
                msg.scope = 'name'
                return msg

            # Include a dot so we obtain members via an omitted target expression
            left += '.'

        # Construct the code with our placeholder for the cursor. It finds unbalanced parens in 
        # the left hand side of the code to try and complete any unclosed statements, the rest of
        # the line where the cursor is placed is commented out.
        unbalanced = GetUnbalancedParens(left)
        code = '{0}__cursor_location__ {1} ; # {2}' % (
            left,
            join(unbalanced, ''),
            right
        )

        # Find proposals for the cursor location
        Index.WithCompiler(query.fname, code) do (module):
            # Query globals
            if not skip_globals and msg.scope != 'members':
                for ent in GlobalsForModule(module):
                    ProcessEntity(msg, ent, query.extra)

            mre as Ast.MemberReferenceExpression = Visitors.IdentFinder('__cursor_location__').FindIn(module)
            if not mre:
                Trace.TraceInformation('MRE not found!')
                return

            # Obtain member proposals
            for ent in Index.MembersOf(mre.Target):
                ProcessEntity(msg, ent, query.extra)

            if not skip_extensions and msg.scope == 'members':
                for ent in GlobalsForModule(module):
                    extent = ent as IExtensionEnabled
                    continue unless extent and extent.IsExtension
                    if IsExtensionOf(mre.Target.ExpressionType, extent):
                        ProcessEntity(msg, extent, query.extra)

            # Query locals
            if msg.scope not in ('members', 'type'):
                # Use cursor expression line if one wasn't explicitly given
                query.line = query.line or mre.LexicalInfo.Line

                # Optimize by finding the top most block
                node as Ast.Node = mre.Target
                while node.ParentNode.NodeType not in (Ast.NodeType.Module, Ast.NodeType.ClassDefinition):
                    node = node.ParentNode

                # Undo the optimization if the line is not inside its scope, this should
                # work around closures being transformed to internal classes by the compiler.
                if query.line < node.LexicalInfo.Line or query.line > node.EndSourceLocation.Line:
                    node = module

                # Collect local variables at the cursor position
                finder = Visitors.LocalsFinder(query.fname, query.line)
                for ent in finder.FindIn(node):
                    ProcessEntity(msg, ent, query.extra)

        # filter results based on prefix
        msg = FilterPrefix(msg, prefix)

        if msg.scope == 'type':
            msg.hints.RemoveAll({ h | h.node not in ('Namespace', 'Type') })

        return msg

    protected def IsExtensionOf(target as IType, ext as IExtensionEnabled):
        if not target or not ext or not ext.IsExtension:
            return false

        params = ext.GetParameters()
        if not len(params):
            return false

        exttype = params[0].Type
        if TypeCompatibilityRules.IsAssignableFrom(exttype, target):
            return true

        # TODO: Check generics
        return false

    virtual def globals(query as Messages.Query) as Messages.Hints:
        msg = Messages.Hints()
        msg.scope = 'globals'

        Index.WithCompiler(query.fname, query.code) do (module):
            for ent in GlobalsForModule(module):
                ProcessEntity(msg, ent, query.extra)

        return msg

    protected def GlobalsForModule(module as Ast.Module):
        # Collect current module members
        for m in module.Members:
            # Globals are wrapped inside a Module class
            if m.Name[-6:] == 'Module':
                for mm in (m as Boo.Lang.Compiler.Ast.TypeDefinition).Members:
                    continue unless mm.IsStatic
                    continue if mm.IsInternal
                    continue if mm.Name == 'Main'
                    yield mm.Entity
                continue

            yield m.Entity

        # Process imported symbols
        refexp = Ast.ReferenceExpression()
        for imp in module.Imports:
            # Handle aliases imports
            if imp.Alias and imp.Alias.Entity:
                yield imp.Alias.Entity
                continue

            # Namespace imports. We fake a member reference expression for the namespace
            refexp.Entity = imp.Entity
            mie = imp.Expression as Ast.MethodInvocationExpression

            entities = Index.MembersOf(refexp)
            for ent in entities:
                # Filter out namespace members not actually imported
                continue if mie and not mie.Arguments.Contains({n as Ast.ReferenceExpression | n.Name == ent.Name})
                yield ent

    protected def DocStringFor(entity as IEntity):
        if target = entity as IInternalEntity:
            if not string.IsNullOrEmpty(target.Node.Documentation):
                return target.Node.Documentation
        return null

    protected def ProcessEntity(msg as Messages.Hints, entity as IEntity):
        ProcessEntity(msg, entity, false)

    protected def ProcessEntity(msg as Messages.Hints, entity as IEntity, extra as bool):
        ProcessEntity(msg, entity, entity.Name, extra)

    protected def ProcessEntity(msg as Messages.Hints, entity as IEntity, name as string, extra as bool):

        # Ignore compiler generated variables
        return if name.StartsWith('$')

        hint = Messages.Hints.Hint()
        hint.name = name
        hint.node = entity.EntityType.ToString()
        hint.full = entity.FullName

        # Unroll ambiguous entities
        if ambiguous = entity as Ambiguous:
            if extra:
                # In extra mode process each entity on its own
                for ent in (entity as Ambiguous).Entities:
                    ProcessEntity(msg, ent, name, extra)
                return

            # In non extra mode report the number of overloads
            hint.info = len(ambiguous.Entities).ToString()
            # For overloaded methods assume all have the same return type
            if ambiguous.AllEntitiesAre(EntityType.Method):
                hint.type = (ambiguous.Entities[0] as IMethodBase).ReturnType.ToString()
            msg.hints.Add(hint)
            return

        if extra:
            hint.doc = DocStringFor(entity)
            hint.loc = Index.GetSourceLocation(entity)

        match entity:
            case t=IType():
                # TODO: for extra include inherited types in params
                info = []
                info.Add('array') if t.IsArray
                info.Add('interface') if t.IsInterface
                info.Add('enum') if t.IsEnum
                info.Add('value') if t.IsValueType
                if t.IsClass:
                    info.Add('class')
                    info.Add('abstract') if t.IsAbstract
                    info.Add('final') if t.IsFinal

                tss = my(TypeSystemServices)
                if tss.IsPrimitive(hint.full):
                    info.Add('primitive')

                hint.info = join(info, ',')

            case ns=INamespace(EntityType: EntityType.Namespace):
                pass
            case p=IProperty():
                hint.type = p.Type.ToString()
            case f=IField():
                hint.type = f.Type.ToString()
            case e=IEvent():
                hint.type = e.Type.ToString()
            case lc=ILocalEntity():
                hint.type = lc.Type.ToString()
            case em=ExternalMethod():
                hint.type = em.ReturnType.ToString()
                if em.IsExtension:
                    hint.info = 'extension'
                if extra:
                    hint.params = List[of string]()
                    try:
                        for em_p in em.GetParameters():
                            hint.params.Add(em_p.Name + ': ' + em_p.Type)
                    except ex:
                        Trace.TraceError(ex.ToString())
            case ie=IInternalEntity():
                if im = ie.Node as Ast.Method and im.ReturnType:
                    hint.type = im.ReturnType.ToString()
                    if ee=ie as IExtensionEnabled and ee.IsExtension:
                        hint.info = 'extension'
                    if extra:
                        hint.params = List[of string]()
                        try:
                            for im_p in im.Parameters:
                                hint.params.Add(im_p.Name + ': ' + im_p.Type)
                        except ex:
                            Trace.TraceError(ex.ToString())
                else:
                    Trace.TraceInformation('internal {0}, {1}' % (entity, entity.EntityType))
                    hint.info = entity.ToString()
            otherwise:
                Trace.TraceInformation('otherwise {0}, {1}' % (entity, entity.EntityType))
                hint.info = entity.ToString()

        msg.hints.Add(hint)
