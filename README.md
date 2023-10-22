
# Inline Anki

For nearly a decade I've daydreamed of writing [SRS](https://en.wikipedia.org/wiki/Spaced_repetition#Software) flashcards in a way that flows together with longer "body text" in any of my files, so that the texts serve as a [single-source-of-truth](https://en.wikipedia.org/wiki/Single_source_of_truth).

The other SRS programs in the Emacs ecosystem ([org-fc](https://github.com/l3kn/org-fc), [org-drill](https://gitlab.com/phillord/org-drill), [org-anki](https://github.com/eyeinsky/org-anki), [pamparam](https://github.com/abo-abo/pamparam), [anki-editor](https://github.com/louietan/anki-editor)) all share a fatal flaw: **they separate the flashcards from normal text** by way of shoving them under a dedicated headline.

I don't want to awkwardly append flashcards to the end of an Org section as a kind of paraphrasis or summary of that section, but to let the flashcards serve *as* paragraphs in their own right!



## Demonstration

We provide four ways to express flashcards.


### Way 1: As list items

Here are five Anki cloze notes (totalling nine cloze deletions), expressed in just five list items:


-   @<sup>1693000409</sup> 🇫🇷 <ins>chien</ins> 🇬🇧 <ins>dog</ins>
-   @<sup>1693000402</sup> 🇫🇷 <ins>maison</ins>  🇬🇧 <ins>house</ins>
-   @<sup>1693000403</sup> 🇫🇷 <ins>reunion</ins>  🇬🇧 <ins>meeting</ins>
-   @<sup>1690300404</sup> 🇫🇷 <ins>legerdemain</ins>  🇬🇧 <ins>sleight-of-hand</ins>
-   @<sup>1690003421</sup> DRY is short for <ins>Don't Repeat Yourself</ins>

No hidden property drawers or anything.  The source Org syntax is five lines too:

    - @^{1693000409} 🇫🇷 _chien_  🇬🇧 _dog_
    - @^{1693000402} 🇫🇷 _maison_  🇬🇧 _house_
    - @^{1693000403} 🇫🇷 _reunion_  🇬🇧 _meeting_
    - @^{1690300404} 🇫🇷 _legerdemain_  🇬🇧 _sleight-of-hand_
    - @^{1690003421} DRY is short for _Don't Repeat Yourself_

The starting @<sup>number</sup> uniquely identifies the note.  Think of the @ glyph as encircling an "a" for "anki"!

As you've guessed, the underlined parts define cloze deletions.


### Way 2: As running paragraphs

The following two paragraphs express two Anki cloze notes (totalling four cloze deletions).

> In English, we have the word-pair truth and lying.  We also have the word-pair <ins>rational</ins> and rationalization: it's as if lying was called <ins>truthization</ins>.<sup>16900030021</sup>
> 
> A gotcha for inline-anki: you must not <ins>hard-wrap</ins> paragraphs.  If you do, inline-anki will only grab <ins>the last line</ins> of the paragraph.<sup>1690986753420</sup>

Note I omitted the @ glyph.  You can do that on line endings.

### Way 3: As list items, with trailing ID

I find it more aesthetic to put the Anki note ID at the end of a list item, when that list item is a member of a longer list of non-flashcard things.

This list expresses, among other things, three Anki cloze notes:

> -   "The third virtue is lightness. Let the winds of evidence blow you about as though you are a leaf, with no direction of your own.  Beware lest you fight a rearguard retreat against the evidence, grudgingly conceding each foot of ground only when forced, feeling cheated.  Surrender to the truth as quickly as you can.  Do this the instant you realize what you are resisting, the instant you can see from which quarter the winds of evidence are blowing against you.  Be faithless to your cause and betray it to a stronger enemy."
> -   Do not think that fairness to all sides means balancing yourself evenly between positions; truth is not handed out <ins>in equal portions</ins> before the start of a debate.<sup>1695193247566</sup>
> -   If you are equally good at explaining <ins>any outcome</ins>, you have zero knowledge.<sup>1695193247617</sup>
> -   That which can be destroyed by the truth should be.  (P. C. Hodgell)
>     -   Corollary: That which the truth nourishes should thrive.
>         -   If the multiverse is real, I desire to <ins>believe the multiverse is real</ins>, and if the multiverse is not real, I desire to believe the multiverse is not real.  Let me not become attached to beliefs I may not want. (application of the <ins>Litany of Tarski</ins>)<sup>1695193247518</sup>


### Way 4: As structure templates

The block below expresses a single Anki cloze note (totalling three cloze deletions).

If you're reading this in a web browser, note that this is what it'll look like in an org-mode buffer.  I had to show it this way because upon export to the web, the block boundaries disappear and you'd never realize there was anything odd about these paragraphs.

    #+begin_flashcard 165193247510
    The _Litany of Tarski_ goes:
    
    1. If the box contains a diamond,
    2. _I desire to believe that the box contains a diamond_;
    3. _If the box does not contain a diamond_,
    4. I desire to believe that the box does not contain a diamond;
    5. Let me not become attached to beliefs I may not want.
    #+end_flashcard

## Set-up

For set-up, please see [the user manual](doc/inline-anki.md) (also available as Info manual after installation, type `C-h i d m inline-anki`).


## Why underlines?

Underlines are rare on the web for a reason: you easily mistake them for hyperlinks, especially if you're colorblind. 

Think of underlines as the **handwriting equivalent of bold text**.  They belong on paper, not on a computer screen.  They're not even permitted in Markdown!  I had to hack this README by using `<ins>` HTML tags.

Since people avoid the underline on computer screens, it's free real estate to load with a new semantic.  "But you just said to avoid it!"  Well, the upside compared to **bold**/*italic*/~strikethrough~, is we get full control of how a cloze deletion should look!  Here's how it looks in my Emacs:

![Example in Emacs, of cloze text inside a box](box-example-source.png)

and the [corresponding page on my website](https://edstrom.dev/sJt8/replacing-guilt#XjWh) looks like this:

![Example in web browser, of cloze text inside a box](box-example-web.png)

See, no mistaking them for hyperlinks now.  To recreate this appearance in Emacs, use this initfile snippet -- which must run before Org loads.

    (defface my-cloze '((t . (:box t))) "Cloze face")
    (setq org-emphasis-alist '(("*" bold)
                               ("/" italic)
                               ("_" my-cloze) ;; new
                               ("=" org-verbatim verbatim)
                               ("~" org-code verbatim)
                               ("+" (:strike-through t))))

As for the web, Org exports underlines to HTML as `<span class="underline">`, so you can control how it ends up looking on your blog with a CSS rule such as the following.

    span.underline {
        text-decoration: none;
        background-color: #bbb;
        padding: 2px;
    }

### You can still use your chosen marker

Whichever emphasis marker you choose, be it _, *, /, +, ~, or =, inline-anki doesn't bar you from using it in general.  A paragraph needs a magic string like `@anki` to be parsed as a flashcard in the first place, without which the marker has no special meaning.

In fact, the default for this package was once **bold**, not underline.  It didn't result in any accidental flashcards.

The reason I switched to underline was the idea of "*invisible*-anki", a future fork of this project that eliminates the note IDs altogether.  With the note IDs gone, it must reserve an emphasis marker that always means cloze.  I'm early-adopting it.


## Roadmap

-   [ ] Allow hard-wrapping
-   [ ] Fix picture/media export
-   Define some expressions for a traditional front-back note-type (instead of cloze-deletion)
    -   [ ] As a parameter for `#+begin_flashcard` &#x2013; interpret the last paragraph as the back side.
    -   [ ] As a single-column or single-row 2-cell table.
        -   No need for `#+begin_flashcard` and no need to add a blank line in between the fields.
        -   [ ] Extend this so that if you have many rows, you can flag the whole table as a table of flashcards where each row is one flashcard.
            -   But that's mainly useful for vocabulary-type stuff, since not much text fits in one table row.


<a id="org7d5eb0b"></a>

## Thanks

Thanks a lot to @louietan who wrote <https://github.com/louietan/anki-editor>.  I could rely on its AnkiConnect-interfacing code, which lowered the barrier for me to get started.

