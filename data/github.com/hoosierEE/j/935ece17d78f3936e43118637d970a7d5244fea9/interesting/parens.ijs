Note 'origin'

I started learning J because of [these](http://prog21.dadgum.com/48.html) [two](http://prog21.dadgum.com/14.html) blog posts.  There's something about programming in J that reminds me of editing in Vim.  In Vim you have a handful of powerful primitives, like `d` (delete) which can take numeric or symbolic (e.g. motions like `hjkl`) operands.  For example, if you want to delete 20 lines down, starting at the cursor position, you'd enter:


    d20j

In J there are similar primitives which operate on all kinds of array data (not just strings), and it's easy to interactively build a more complex program from simpler elements.

    ('()' i.])  NB. replace elements of the right argument with 0 (if it's an open-paren), 1 (if it's a close-paren), or 2 (if neither)
    ((1 _1 0) {~ '()'i.])  NB. 0 1 and 2 become indexes into a new array (1 _1 0)
    ([:+/\(1 _1 0) {~ '()' i.])  NB. running sum of parentheses nesting level

From there you can determine if the string contains balanced parentheses:

    (0={:)@([:+/\(1 _1 0){~'()' i.])  NB. does number of open and close parens match?
    (+./0>)@([:+/\(1 _1 0){~'()'i.])  NB. too many closing parens?
    ((0={:)*./@,(0<:])) ([:+/\(1 _1 0){~'()'i.])  NB. are the parens balanced AND matched?

At this point you could go golfing:

    0((={:)*./@,<:) ([:+/\(1 _1 0){~'()'i.])  NB. factor out the 0 by rewriting as a fork
    0(=&{:*./@,<:) ([:+/\(1 _1 0){~'()'i.])  NB. bond `=` with `{:` to make it a single verb and eliminate parens

...or attach names to things to make it easier to understand what the program is meant to do:

)


    matched=.0&(=&{: *./@, <:)
    parens=.[: +/\ 1 _1 0 {~ '()' i. ]
    NB. usage
    NB. matched parens '((())()()'
    NB. returns 0 for not matched or 1 for matched

