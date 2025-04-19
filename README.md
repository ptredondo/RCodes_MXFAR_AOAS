# RCodes_MXFAR_AOAS
This repository contains the data and R codes needed to reproduce key results of the paper entitled "Functional-Coefficient Models for Multivariate Time Series in Designed Experiments: with Applications to Brain Signals".

The contents of ``\texttt{R Codes for MXFAR.R}" are as follows:
\begin{enumerate}
    \item Installing all required packages.\\
    \item Loading user-defined functions for visualization, estimation and plot settings.\\
    \item Loading EEG data and plotting some EEG series.\\
    \item EEG analysis using the MXFAR-\textit{f}PDC framework including:
        \begin{itemize}
             \item model selection based on the Accumulated Prediction Error (APE) criterion,
             \item testing for presence of nonlinearity in the EEG series via the nonparametric test,
             \item estimation of the MXFAR model and the \textit{f}PDC metric for the two groups,
             \item producing connectivity plots (see figures in the main paper),
             \item producing \textit{f}PDC plots for all time windows considered (see Supplementary Material),
             \item sensitivity analysis on the choice of channels, i.e., estimating leave-one-channel-out networks (see Supplementary Material), and
             \item deriving EEG networks based on \textit{significant} and consistent connections (see Supplementary Material).\\
        \end{itemize}

    \item EEG analysis using the linear VAR-PDC framework including:
        \begin{itemize}
             \item estimation of the linear VAR model and the PDC metric for the two groups,
             \item producing connectivity plots (see figures in the main paper), and
             \item deriving EEG networks based on \textit{significant} and consistent connections (see Supplementary Material).\\
        \end{itemize}

    \item EEG analysis using the spectral conditional GGC framework including:
        \begin{itemize}
             \item estimation of the spectral conditional GGC metric for the two groups, and
             \item producing connectivity plots (see figures in the main paper).
        \end{itemize}

    \item Illustrations for the \textit{f}PDC metric including:
        \begin{itemize}
            \item example 1 with sigmoidal functional coefficients, and
            \item example 2 with exponential functional coefficients (see Section 4 of main paper).\\
        \end{itemize}

    \item Run times of the local linear estimation for the MXFAR model.\\

    \item Example codes for conducting simulation study including;
    \begin{itemize}
        \item simulating data from an MXFAR process,
        \item model selection based on the APE criterion, and
        \item testing for nonlinearity based on the proposed nonparametric test.\\
    \end{itemize}
\end{enumerate}
