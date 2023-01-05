%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cascade Analytics Working Paper
% LaTeX Class
% Version 1.0 (January 3, 2023)
%
% This class originates from:
% https://www.LaTeXTemplates.com
%
% Author:
% Anthony Perez Eisenbarth
%
% License:
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------------------------
%	CLASS CONFIGURATION
%----------------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{CascadeAnalyticsWorkingPaper}[2023/01/04 Cascade Analytics Working Paper v1.0]

\usepackage{etoolbox} % Required for conditional logic and easily changing commands

\newtoggle{unnumberedsections} % Create toggle for a class option
\settoggle{unnumberedsections}{false} % Default value for the class option
\DeclareOption{unnumberedsections}{\settoggle{unnumberedsections}{true}} % Set the class option toggle if the class option was used in the template

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}} % Pass through any extra options specified to the base class
\ProcessOptions\relax % Process class options

\LoadClass{article} % Load the base class

%----------------------------------------------------------------------------------------
%	REQUIRED PACKAGES AND MISC CONFIGURATIONS
%----------------------------------------------------------------------------------------

\usepackage{graphicx} % Required for including images
\graphicspath{{Images/}{./}} % Specifies where to look for included images (trailing slash required)
\usepackage{subcaption}
\usepackage{textpos}
\usepackage[figuresleft]{rotating}
\setlength{\parindent}{0pt} % Paragraph indentation
\setlength{\parskip}{10pt} % Vertical space between paragraphs

\usepackage{lastpage} % Required to determine the total number of pages

\AtBeginEnvironment{quote}{\itshape\rmfamily} % Style quotations

\usepackage{appendix} % Required for the appendices section
\usepackage{setspace}
\singlespacing
\usepackage{float} % Allows more precisely positioning floats e.g. \begin{figure}[H]

%----------------------------------------------------------------------------------------
%	MARGINS
%----------------------------------------------------------------------------------------

\usepackage[
	top=20mm, % Top margin
	bottom=20mm, % Bottom margin
	width = 145mm, 
	marginparwidth=4.25cm, % Width of the large right margin (sidebar) content
	marginparsep=1cm, % Padding between the main text and the large right margin (sidebar)
	footskip=1.5cm, % Space from the bottom margin to the baseline of the footer
	headsep=0.8cm, % Space from the top margin to the baseline of the header
	headheight=1cm, % Height of the header
	%showframe % Uncomment to show frames around the margins for debugging purposes
]{geometry}
\usepackage{ragged2e}
\usepackage[hang,flushmargin, bottom]{footmisc}

\newcommand\blfootnote[1]{%
	\begingroup
	\renewcommand\thefootnote{}\footnote{#1}%
	\addtocounter{footnote}{-1}%
	\endgroup
}

%----------------------------------------------------------------------------------------
%	FONTS
%----------------------------------------------------------------------------------------

\usepackage[utf8]{inputenc} % Required for inputting international characters
\usepackage[T1]{fontenc} % Output font encoding for international characters

\usepackage{nimbusserif} % Use nimbus for math
\usepackage{amsfonts, amsmath, amssymb}
\usepackage{relsize}
\usepackage{roboto} % Use Roboto as the sans font 
\usepackage{newtxtext} % Use tx serif as the serif font
\usepackage{plex-mono} % Use IBM Plex Mono as the mono font

\newcommand{\textel}[1]{{\fontseries{el}\selectfont #1}} % Define a simple command for using the ExtraLight weight
\newcommand{\textl}[1]{{\fontseries{l}\selectfont #1}} % Define a simple command for using the Light weight
\newcommand{\textsb}[1]{{\fontseries{sb}\selectfont #1}} % Define a simple command for using the SemiBold weight

%----------------------------------------------------------------------------------------
%	HEADERS AND FOOTERS
%----------------------------------------------------------------------------------------

\usepackage{fancyhdr} % Required for customizing headers and footers
\pagestyle{fancy} % Enable custom headers and footers

\renewcommand{\headrulewidth}{0.5pt} % Top horizontal rule thickness

\fancyhf{} % Clear default headers/footers

\fancyhead[L]{\reporttitle} % Left header
\fancyhead[C]{} % Center header
\fancyhead[R]{} % Right header

\fancyfoot[L]{} % Left footer
\fancyfoot[C]{\thepage~of \pageref{LastPage}} % Center footer
\fancyfoot[R]{\includegraphics[width=1.5cm]{company.png}} % Right footer


%----------------------------------------------------------------------------------------
%	SECTIONS
%----------------------------------------------------------------------------------------

\usepackage{titlesec} % Required for modifying sections

\numberwithin{equation}{section}

\iftoggle{unnumberedsections}{ % Conditional logic for the unnumbered sections class options
	\setcounter{secnumdepth}{0} % Don't number sections at any level
}{
	\setcounter{secnumdepth}{3} % Number sections down to subsubsections
}

\titleformat
	{\section} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\LARGE\sffamily\mdseries\newpage} % Text formatting of the whole section, i.e. label and title
	{\thesection} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\section}{0pt}{\baselineskip}{4pt} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\subsection} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\large\sffamily\mdseries} % Text formatting of the whole section, i.e. label and title
	{\thesubsection} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\subsection}{0pt}{0.75\baselineskip}{4pt} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\subsubsection} % Section type being modified
	[block] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\raggedright\normalsize\sffamily\mdseries} % Text formatting of the whole section, i.e. label and title
	{\thesubsubsection} % Section label (e.g. number) and its formatting
	{5pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[] % Code after the section title

\titlespacing*{\subsubsection}{0pt}{0.5\baselineskip}{0pt} % Spacing around section titles, the order is: left, before and after

%------------------------------------------------

\titleformat
	{\paragraph} % Section type being modified
	[runin] % Section layout type, can be: hang, block, display, runin, leftmargin, rightmargin, drop, wrap, frame
	{\bfseries} % Text formatting of the whole section, i.e. label and title
	{} % Section label (e.g. number) and its formatting
	{0pt} % Horizontal space between the section label and title
	{} % Code before the section title
	[~~~~] % Code after the section title

\titlespacing*{\paragraph}{0pt}{0pt}{0pt} % Spacing around section titles, the order is: left, before and after

%----------------------------------------------------------------------------------------
%	TABLE OF CONTENTS
%----------------------------------------------------------------------------------------

\usepackage{titletoc} % Required for manipulating the table of contents

\setcounter{tocdepth}{2} % Show entries in the table of contents down to subsections

\renewcommand{\contentsname}{Table of Contents} % Rename the table of contents section heading

\titlecontents{section} % Section type being modified
	[0pt] % Left indentation
	{\vspace{-2pt}} % Before code
	{\sffamily{\thecontentslabel}~~\sffamily} % Formatting of numbered sections of this type
	{\sffamily} % Formatting of numberless sections of this type
	{\titlerule*[6pt]{.}\textsb{\contentspage}} % Formatting of the filler to the right of the heading and the page number
	[\vspace{2pt}] % After code

%------------------------------------------------

\titlecontents{subsection} % Section type being modified
	[0.5cm] % Left indentation
	{\vspace{-8pt}} % Before code
	{\sffamily{\thecontentslabel}~~} % Formatting of numbered sections of this type
	{} % Formatting of numberless sections of this type
	{\titlerule*[6pt]{.}\contentspage} % Formatting of the filler to the right of the heading and the page number
	[] % After code
	
%------------------------------------------------

\titlecontents{subsubsection} % Section type being modified
	[1.35cm] % Left indentation
	{\vspace{-4pt}} % Before code
	{\sffamily{\thecontentslabel}~~} % Formatting of numbered sections of this type
	{} % Formatting of numberless sections of this type
	{\titlerule*[6pt]{.}\contentspage} % Formatting of the filler to the right of the heading and the page number
	[] % After code


%----------------------------------------------------------------------------------------
%	COLORS
%----------------------------------------------------------------------------------------

\usepackage[usenames, dvipsnames]{xcolor} % Required for defining and using custom colors

\definecolor{magenta}{HTML}{BE3455}
\definecolor{emerald}{HTML}{00997b}
\definecolor{classic_blue}{HTML}{0F4C81}

%----------------------------------------------------------------------------------------
%	BIBLIOGRAPHY
%----------------------------------------------------------------------------------------




%----------------------------------------------------------------------------------------
%	TABLES
%----------------------------------------------------------------------------------------

\usepackage{booktabs} % Required for better horizontal rules in tables

\usepackage{array} % Required for manipulating table columns
\renewcommand{\arraystretch}{1.25} % Increase the height of table rows

\newcolumntype{R}[1]{>{\raggedleft\arraybackslash}p{#1}} % Define a new right-aligned paragraph column type
\newcolumntype{L}[1]{>{\raggedright\arraybackslash}p{#1}} % Define a new left-aligned (no justification) paragraph column type
\newcolumntype{C}[1]{>{\centering\arraybackslash}p{#1}} % Define a new centered paragraph column type

%----------------------------------------------------------------------------------------
%	CAPTIONS
%----------------------------------------------------------------------------------------

\usepackage[labelfont = sc]{caption} % Required for customizing captions

\captionsetup{skip=6pt} % Vertical whitespace between figures/tables and the caption (default is 10pt)
\captionsetup{justification=raggedright, singlelinecheck=false} % Left-align captions

% The sidenotes package defines 4 custom caption styles for wide tables, wide figures, margin tables and margin figures
% To style these the same as in-text tables and figures, set these 4 styles to the same styling values
\DeclareCaptionStyle{widetable}{font={bf, footnotesize}, justification=raggedright, singlelinecheck=false}
\DeclareCaptionStyle{widefigure}{font={bf, footnotesize}, justification=raggedright, singlelinecheck=false}
\DeclareCaptionStyle{marginfigure}{font={bf, scriptsize}, justification=raggedright, singlelinecheck=false}
\DeclareCaptionStyle{margintable}{font={bf, scriptsize}, justification=raggedright, singlelinecheck=false}

%----------------------------------------------------------------------------------------
%	LISTS
%----------------------------------------------------------------------------------------

\usepackage{enumitem} % Required for list customization

\setlist{itemsep=0pt, topsep=0pt} % Customize spacing around and inside lists

\renewcommand{\labelenumi}{\arabic{enumi}.} % Customize the first level numbered list characters
\renewcommand{\labelenumii}{\alph{enumii}.} % Customize the second level numbered list characters
\renewcommand{\labelenumiii}{\roman{enumiii}.} % Customize the third level numbered list characters

\renewcommand{\labelitemi}{\Large\raisebox{-1.5pt}{\textbullet}} % Customize the first level bullet point character
\renewcommand{\labelitemii}{\Large\raisebox{-1.5pt}{--}} % Customize the second level bullet point character
\renewcommand{\labelitemiii}{\Large\raisebox{-1.5pt}{-}} % Customize the third level bullet point character

%----------------------------------------------------------------------------------------
%	LINKS
%----------------------------------------------------------------------------------------

\usepackage{hyperref} % Required for links

\hypersetup{
	colorlinks=true, % Whether to  the text of links
	urlcolor=magenta, % Color for \url and \href links
	linkcolor=classic_blue, % Color for \nameref links
	citecolor=magenta, % Color of reference citations
}

%----------------------------------------------------------------------------------------
%	CODE LISTINGS
%----------------------------------------------------------------------------------------

\usepackage{listings} % Required for formatted code blocks

\lstset{
	numbers=left, % Line numbers location
	numbersep=6pt, % Whitespace to the right of line numbers
	numberstyle=\tiny\it, % Line numbers font style
	frame=single, % Border
	basicstyle=\tt\scriptsize, % Code font style
	breaklines=true, % Break long lines into multiple lines automatically
	aboveskip=1.5\baselineskip, % Vertical whitespace before the listing
	tabsize=3, % How many space to a tab
}

%----------------------------------------------------------------------------------------
%	CUSTOM COMMANDS
%----------------------------------------------------------------------------------------
%\renewcommand{\bibsection}{\section{References}}
\newcommand{\reporttitle}[1]{\renewcommand{\reporttitle}{#1}}
\newcommand{\reportsubtitle}[1]{\renewcommand{\reportsubtitle}{#1}}
\newcommand{\reportauthors}[1]{\renewcommand{\reportauthors}{#1}}
\newcommand{\reportdate}[1]{\renewcommand{\reportdate}{#1}}