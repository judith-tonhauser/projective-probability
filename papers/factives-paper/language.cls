%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% LaTeX class file language.cls
%
% Intended for the journal Language (Linguistic Society of America)
% Created by John Beavers on 01/04/2021
%
% This is an attempt to translate the Language style file into a latex
% class file. The style sheet can be found here:
%
% https://www.linguisticsociety.org/sites/default/files/style-sheet.pdf
%
% I have tried to incorporate all global aspects of the style file
% that do not depend on a particular package here, including line
% spacing, lack of full justification, page number placement, page
% breaks, and section header formatting, among others. It also
% incorporates the language.bst file for correct formatting of the
% bibliography. However, there are several things it does not do. It
% does not handle the lack of parentheses around references to
% numbered examples, since how parentheses are done depends on an
% author's choice of example package. It also does not handle how
% glosses are formatted, for the same reason, or ensure correct use of
% single vs. double quotes, etc., this is a matter of individual
% papers. It also does not place figures and tables at the end, since
% individual authors will need to decide how this is done.
% 
% I have defined or redefined a small number of commands and
% environments often called by the user.
%
% - \section, \subsection, \subsubsection - Redefined to fit the style
% sheet.
%
% - \fsubsection, \fsubsubsection - New variants of \subsection
% and \subsubsection that do not put space between the preceding
% material and the new header, designed for use when immediately
% following a previous section header.
%
% - \ackfootnote - A command to put a starred footnote at the end of the
% abstract.
%
% - \footnote has been redefined as \endnote so you can keep using the
% \footnote command, though \ackfootnote gets around that and makes a
% real footnote. You can of course also use \endnote.
%
% - \keywords - Produces formatted keywords
%
% - \maketitle - Produces a title page with authors and then one
% without, as per stylesheet
%
% - Abstract environment - Redefined for proper formatting and to keep
% abstract on separate page from the rest of the document. 
%
% - \listoffigures - Redefined for formatting and pagination.
%
% - \listoftables - Redefined for formatting and pagination.
%
% - \fendnotes - For producing the endnotes at the end, formatted
% appropriately.
% 
% Other commands have been redefined that are called by other things
% to accord with the style sheet.
%
% For the future: fix the sectioning commands so that if a
% (sub)subsection follows a (sub)section it automatically does not put
% a space between them.
%
% Please feel free to email me if you have questions or comments. 
% 
% John Beavers
% Co-Editor, Language
% Professor of Linguistics, The University of Texas at Austin
% jtbeavers [at] utexas.edu
% https://sites.google.com/a/utexas.edu/jbeavers/home
%
% Note that while I am a long-term, devout LaTeX user and also a
% former computer programmar, I am not a deep in the weeds LaTeX
% expert, I have never made a class file before, and my regular
% programming days are two decades behind me. So this class file is
% probably not done using the best LaTeX technique. So if anyone has
% suggestions on how to do things better, or wants to collaborate to
% make a better class file, or even just redo it better based on this
% (provided the original gets acknowledged), feel free.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{language}[2021/01/01 Language class file v1.0]
\LoadClass[12pt]{article}
\RequirePackage{setspace}
\RequirePackage[justification=centering]{caption}
\RequirePackage[none]{hyphenat} 
\RequirePackage{ragged2e}
\RequirePackage{fancyhdr}
\RequirePackage{natbib}
\RequirePackage{endnotes}
\RequirePackage{xifthen}

%Appropriate line spacing
\onehalfspacing

%Appropriate bibliography settings
\bibpunct[: ]{(}{)}{,}{a}{}{,}
\bibliographystyle{language}

%Standard margins with room for page numbers
\topmargin -30pt
\oddsidemargin 0in
\headheight 15pt
\headsep 15pt
\textheight 9in
\textwidth 6.5in

%Header has page number on right margin, starting at 0
\fancyhf{}
\fancyhead[R]{\thepage}
\fancyfoot{}
\pagestyle{fancy}
\renewcommand{\headrulewidth}{0pt}
\setcounter{page}{0}

%Formatting for captions and titles
\captionsetup[figure]{labelfont=sc,justification=raggedright,labelsep=period}
\captionsetup[table]{labelfont=sc,justification=raggedright,labelsep=period}

%This seems to produce the right justification decisions
\setlength{\RaggedRightParindent}{\parindent}
\RaggedRight
\sloppy

%No double spaces after punctuation
\frenchspacing

% %Language's unique section headers
\makeatletter

\renewcommand{\@seccntformat}[1]{\bfseries\csname the#1\endcsname.\ \normalfont\sc}

\renewcommand{\section}[1]{\@startsection
{section}
{1}
{0mm}
{1em}
{0mm}
{\normalfont\sc}{#1.\space}
}

\renewcommand{\subsection}[1]{\@startsection
{subsection}
{2}
{0mm}
{1em}
{0mm}
{\normalfont\normalsize\sc}{#1.\space}
}

\renewcommand{\subsubsection}[1]{\@startsection
{subsection}
{2}
{0mm}
{1em}
{0mm}
{\normalfont\normalsize\sc}*{#1. \ }
}


\newcommand{\fsubsection}[1]{\@startsection
{subsection}
{2}
{0mm}
{0em}
{0mm}
{\normalfont\normalsize\sc}{#1.\space}
}

\newcommand{\fsubsubsection}[1]{\@startsection
{subsection}
{2}
{0mm}
{0em}
{0mm}
{\normalfont\normalsize\sc}*{#1. \ }
}

\renewcommand{\enoteheading}{\clearpage\@startsection
{section}
{2}
{0mm}
{0.5\baselineskip}
{0.1\baselineskip}
{\normalfont\normalsize\sc}*{\centering NOTES}
}

\renewcommand{\bibsection}{\clearpage\@startsection
 {section}
 {1}
 {0mm}
 {0mm}
 {0mm}
 {\normalfont\normalsize\sc}*{\begin{center}REFERENCES\end{center}}
 }

\renewcommand{\listoffigures}{%
    \section{List of figures}\ \newline
    \@starttoc{lof}%
    }

\renewcommand{\listoftables}{%
    \section{List of tables}\ \newline
    \@starttoc{lot}%
    }
\makeatother

%Font and spacing for endnotes
\newcommand{\fendnotes}{
\begingroup
\parindent 0pt
\parskip 2ex
\def\enotesize{\normalsize}
\theendnotes
\endgroup
}

\let\oldfootnote=\footnote
\let\footnote=\endnote

%A special command for the acknowledgments
\newcommand{\ackfootnote}[1]{\let\footnote=\oldfootnote%
  \renewcommand\thefootnote{*}{\footnote{#1}}%
\let\footnote=\endnote}

%no rule in the one footnote
\renewcommand\footnoterule{}

\makeatletter
\renewcommand{\maketitle}{%
   \begin{center}%
%      \sffamily
      {\LARGE\@title\par}%
      \bigskip
      \large \lineskip 1em%
  \begin{tabular}[t]{c}
      \@author%
      \end{tabular}\par
      \bigskip
      {\large\@date\par}%
%      \bigskip\hrule\vspace*{2pc}%
   \end{center}%
\ \newpage
\begin{center}
{\LARGE\@title\par}
\end{center}
\ \newpage
}
\makeatother

%Redefine abstract to use small caps and also add a newpage
\renewenvironment{abstract}{
\begin{center}
{\sc Abstract}
\end{center}
\begin{quote}
}
{\end{quote}\newpage}

%Formats keywords nicely
\newcommand{\keywords}[1]{\medskip\noindent{\it Keywords:\space} #1}

