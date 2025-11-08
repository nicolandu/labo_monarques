% TODO: update dependencies based on bio.cls
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{bio_presentation, 9pt}[Brebeuf bio presentation]

\LoadClass[aspectratio=169]{beamer}
\input{beamer_notes_patch}

% Language and quotes
\RequirePackage[french]{babel}
\RequirePackage{csquotes}

% Allows having Aboxed, the aligned version of boxed.
\RequirePackage{mathtools}

\RequirePackage{fontspec}
\setsansfont{Fira Sans}[
    SmallCapsFeatures={Letters=SmallCaps}
]

\RequirePackage{amsfonts}
\RequirePackage{amssymb}
\RequirePackage{float}

\RequirePackage{tikz}

\RequirePackage[output-decimal-marker={,}, per-mode=reciprocal, forbid-literal-units]{siunitx}

\RequirePackage[style=apa, urldate=iso, date=iso, seconds=true, language=french]{biblatex}

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
\defbibheading{custom}{}

\AtEveryBibitem{
  \clearfield{labelday}
} \makeatletter
\let\beamer@writeslidentry@miniframeson=\beamer@writeslidentry
\def\beamer@writeslidentry@miniframesoff{%
  \expandafter\beamer@ifempty\expandafter{\beamer@framestartpage}{}% does not happen normally
  {%else
    % removed \addtocontents commands
    \clearpage\beamer@notesactions%
  }
}
\newcommand*{\miniframeson}{\let\beamer@writeslidentry=\beamer@writeslidentry@miniframeson}
\newcommand*{\miniframesoff}{\let\beamer@writeslidentry=\beamer@writeslidentry@miniframesoff}
\beamer@compresstrue

\let\@oldprintbibliography\printbibliography
\renewcommand{\printbibliography}{
    \miniframesoff
    \section{Médiagraphie}
    \begin{frame}[allowframebreaks]
        \frametitle{Médiagraphie}
        \@oldprintbibliography[heading=custom]
    \end{frame}
    \miniframeson
}
\makeatother

\RequirePackage{icomma} % no space after decimal comma

\usetikzlibrary{babel}

% Non-breaking thin space for guillemets
\frenchbsetup{StandardLists=true, og=«, fg=»}

% Beamer
\beamertemplatenavigationsymbolsempty
\usefonttheme[onlymath]{serif}
\usetheme{Dresden}
\usecolortheme{beaver}
\setbeamertemplate{bibliography item}[text]
\setbeamertemplate{frametitle continuation}{}
\AtBeginDocument{
    \sisetup{
        mode=text,
        text-font-command=\rmfamily
    }
}
