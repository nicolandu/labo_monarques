\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{bio}[Brebeuf bio lab report]

\LoadClass[letterpaper]{article}

\RequirePackage{calc} % first, so that others have the good skips

\RequirePackage[top=30mm, left=40mm, bottom=25mm, right=25mm]{geometry}

\setlength{\smallskipamount}{2\smallskipamount}
\setlength{\medskipamount}{2\medskipamount}
\setlength{\bigskipamount}{2\bigskipamount}

\RequirePackage{titlesec}

% Language and quotes
\RequirePackage[french]{babel}
\RequirePackage{csquotes}

\RequirePackage[skip]{parskip}

\RequirePackage{tocloft}
\renewcommand{\cftdotsep}{1} % spacing between dots
\renewcommand{\cftsecleader}{\cftdotfill{\cftdotsep}}
\renewcommand{\cftsubsecleader}{\cftdotfill{\cftdotsep}}
\renewcommand{\cftsubsubsecleader}{\cftdotfill{\cftdotsep}}

% Allows having Aboxed, the aligned version of boxed.
\RequirePackage{mathtools}

\RequirePackage{amsfonts}
\RequirePackage{amssymb}
\RequirePackage{float}

\RequirePackage{multicol}

\RequirePackage{tikz}

\RequirePackage[labelsep=colon]{caption}

\RequirePackage[output-decimal-marker={,}, per-mode=reciprocal, forbid-literal-units]{siunitx}

\RequirePackage[style=apa, sortcites, language=french]{biblatex}

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

\RequirePackage{icomma} % no space after decimal comma

\RequirePackage{enumitem}

\usetikzlibrary{babel}


% Hyperlinks (always last)
\RequirePackage[hidelinks, pdfpagemode=UseOutlines, bookmarksopen=true, bookmarksnumbered=true, bookmarksdepth=2]{hyperref}

% Non-breaking thin space for guillemets
\frenchbsetup{StandardLists=true, og=«, fg=»}


% Level 1
\setlist[enumerate,1]{label=\arabic*., ref=\it\arabic*}

% Level 2
\setlist[enumerate,2]{label=\alph*), ref=\it\alph*}

% Level 3
\setlist[enumerate,3]{label=\roman*., ref=\it\roman*}

% Level 4
\setlist[enumerate,4]{label=\Alph*., ref=\it\Alph*}

\makeatletter
\newcommand{\teacher}[1]{
	\def\@teacher{#1}
}
\newcommand{\course}[1]{
	\def\@course{#1}
}
\newcommand{\department}[1]{
	\def\@department{#1}
}
\newcommand{\group}[1]{
	\def\@group{#1}
}

\renewcommand{\maketitle}{%
    \setcounter{page}{0}
    \thispagestyle{empty}
    \begin{center}
        \@author
        \vfill
        \@title
        \vfill
        Présenté à\\
        \@teacher \\
        Cours \@course \\
        Groupe \@group
        \vfill
        Collège Jean-de-Brébeuf \\
        Département de \@department \\
        \@date
    \end{center}
    \clearpage
}
\makeatother

\newcommand{\tocpage}{
    \pagenumbering{roman}
    \setcounter{page}{1}
    \makeatletter
    \tableofcontents
    \makeatother
    \clearpage
    \pagenumbering{arabic}
    \setcounter{page}{1}
}
