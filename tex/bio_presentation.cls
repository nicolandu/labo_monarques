\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{bio_presentation}[Brebeuf bio presentation]

\LoadClass[aspectratio=169]{beamer}

% Language and quotes
\RequirePackage[french]{babel}
\RequirePackage{csquotes}

% Allows having Aboxed, the aligned version of boxed.
\RequirePackage{mathtools}

\RequirePackage{amsfonts}
\RequirePackage{amssymb}
\RequirePackage{float}

\RequirePackage{tikz}

\RequirePackage[labelsep=colon]{caption}

\RequirePackage[output-decimal-marker={,}, per-mode=reciprocal, forbid-literal-units]{siunitx}

\RequirePackage[style=apa, urldate=iso, date=iso, seconds=true, autolang=none]{biblatex}
\renewcommand*{\multicitedelim}{,} % no space in [1,2,3]
\setlength{\bibitemsep}{\parskip} % spacing between entries
\setlength{\bibparsep}{0pt} % spacing between paragraphs in a single entry
\DeclareFieldFormat{eprint:hdl}{%
    \iflanguage{french}{hdl}{HDL}\addcolon\space
    \ifhyperref
        {\href{https://hdl.handle.net/#1}{\nolinkurl{#1}}}
        {\nolinkurl{#1}}}

\makeatletter % arXiv seems to have an *explicit, non-configurable* `\texttt'

\DeclareFieldFormat{eprint:arxiv}{%
  arXiv\addcolon\space
  \ifhyperref
    {\href{https://arxiv.org/\abx@arxivpath/#1}{%
       \nolinkurl{#1}%
       \iffieldundef{eprintclass}
         {}
         {\addspace\mkbibbrackets{\thefield{eprintclass}}}}}
    {\nolinkurl{#1}%
     \iffieldundef{eprintclass}
       {}
       {\addspace\mkbibbrackets{\thefield{eprintclass}}}}}

\makeatother

\DeclareCiteCommand{\citetitleplain}
  {\usebibmacro{prenote}}
  {\iffieldundef{shorttitle}{\thefield{title}}{\thefield{shorttitle}}}
  {\multicitedelim}
  {\usebibmacro{postnote}}
\DeclareCiteCommand{\citetitleplain}
  {\usebibmacro{prenote}}
  {\thefield{title}}
  {\multicitedelim}
  {\usebibmacro{postnote}}

\RequirePackage{icomma} % no space after decimal comma

\RequirePackage{enumitem}

\usetikzlibrary{babel}

% Non-breaking thin space for guillemets
\frenchbsetup{StandardLists=true, og=«, fg=»}

% Beamer
\usetheme{Dresden}
\usecolortheme{beaver}
