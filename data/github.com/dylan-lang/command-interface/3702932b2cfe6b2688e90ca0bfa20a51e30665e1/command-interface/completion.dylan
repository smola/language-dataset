module: command-interface
synopsis: CLI completion.
author: Ingo Albrecht <prom@berlin.ccc.de>
copyright: see accompanying file LICENSE

/**
 * Represents the result of completing a node,
 * possibly hinted by a pre-existing token.
 */
define class <command-completion> (<object>)
  /* node the completion was performed for */
  constant slot completion-node :: <parse-node>,
    required-init-keyword: node:;
  /* Value placeholder for help */
  constant slot completion-help-symbol :: false-or(<string>) = #f,
    init-keyword: help-symbol:;
  /* Main help text */
  constant slot completion-help-text :: false-or(<string>) = #f,
    init-keyword: help-text:;
  /* token used to hint the completion, if provided */
  constant slot completion-token :: false-or(<command-token>) = #f,
    init-keyword: token:;
  /* was this completion exhaustive?
     (if yes then only given complete options are valid) */
  constant slot completion-exhaustive? :: <boolean> = #f,
    init-keyword: exhaustive?:;
  /* actual completion options */
  constant slot completion-options :: <list> = #(),
    init-keyword: options:;
end class;

/**
 * Initializes back-references in completion options
 */
define method initialize (completion :: <command-completion>,
                          #rest args, #key, #all-keys)
 => ();
  next-method();
  // initialize reverse links
  for (option in completion.completion-options)
    option-completion(option) := completion;
  end;
end method;

/**
 * Represents a single option returned by completion
 *
 * An option may be COMPLETE, which means that it represents
 * a syntactically complete parameter value which can be
 * used as-is, whereas INCOMPLETE options are not valid
 * values.
 */
define class <command-completion-option> (<object>)
  /* normally initialized by make(<completion>) */
  slot option-completion :: false-or(<command-completion>) = #f;
  /* string for this option */
  constant slot option-string :: <string>,
    required-init-keyword: string:;
  /* true if this option is complete */
  constant slot option-complete? :: <boolean> = #f,
    init-keyword: complete?:;
end class;

/**
 * Construct a completion result
 */
define function make-completion (node :: <parse-node>,
                                 token :: false-or(<command-token>),
                                 #key exhaustive? :: <boolean> = #f,
                                      complete-options :: <sequence> = #(),
                                      other-options :: <sequence> = #())
  => (completion :: <command-completion>);
  // get node help strings
  let help-symbol = node-help-symbol(node);
  let help-text = node-help-text(node);
  // apply token restrictions
  if (token)
    let tokstr = token-string(token);
    // filter options using token
    complete-options := choose(rcurry(starts-with?, tokstr), complete-options);
    other-options := choose(rcurry(starts-with?, tokstr), other-options);
    // add token as an incomplete option for non-exhaustive completion
    if (~exhaustive?)
      let all-options = concatenate(complete-options, other-options);
      unless (member?(tokstr, all-options, test: \=))
        other-options := add!(other-options, tokstr);
      end;
    end;
  end;
  // add longest common prefix as an incomplete option,
  // but filter it against existing options and the token
  let all-options = concatenate(complete-options, other-options);
  let lcp = longest-common-prefix(all-options);
  unless (empty?(lcp) | member?(lcp, all-options, test: \=))
    unless (token & lcp = token-string(token))
      other-options := add!(other-options, lcp);
    end;
  end;
  // construct the result
  local method as-complete-option(string :: <string>)
          make(<command-completion-option>, string: string, complete?: #t);
        end,
        method as-other-option(string :: <string>)
          make(<command-completion-option>, string: string);
        end;
  make(<command-completion>,
       node: node, token: token,
       exhaustive?: exhaustive?,
       help-symbol: help-symbol,
       help-text: help-text,
       options: concatenate-as(<list>,
                               map(as-complete-option, complete-options),
                               map(as-other-option, other-options)));
end;
