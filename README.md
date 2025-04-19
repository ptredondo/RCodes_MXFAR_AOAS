# RCodes_MXFAR_AOAS
This repository contains the data and R codes needed to reproduce key results of the paper entitled "Functional-Coefficient Models for Multivariate Time Series in Designed Experiments: with Applications to Brain Signals" by Paolo Victor Redondo, Raphael Huser and Hernando Ombao.

The contents of ``R Codes for MXFAR.R" are as follows:

1. Installing all required packages.
2. Loading user-defined functions for visualization, estimation and plot settings.
3. Loading EEG data and plotting some EEG series.
4. EEG analysis using the MXFAR-fPDC framework including:
   (a) model selection based on the Accumulated Prediction Error (APE) criterion,
   (b) testing for presence of nonlinearity in the EEG series via the nonparametric test,
   (c) estimation of the MXFAR model and the fPDC metric for the two groups
   (d) producing connectivity plots (see figures in the main paper),
   (e) producing fPDC plots for all time windows considered (see Supplementary Material),
   (f) sensitivity analysis on the choice of channels, i.e., estimating leave-one-channel-out networks (see Supplementary Material), and
   (g) deriving EEG networks based on significant and consistent connections (see Supplementary Material).
5. EEG analysis using the linear VAR-PDC framework including:
   (a) estimation of the linear VAR model and the PDC metric for the two groups,
   (b) producing connectivity plots (see figures in the main paper), and
   (c) deriving EEG networks based on significant and consistent connections (see Supplementary Material).
6. EEG analysis using the spectral conditional GGC framework including:
   (a) estimation of the spectral conditional GGC metric for the two groups, and
   (b) producing connectivity plots (see figures in the main paper), and
7. Illustrations for the \textit{f}PDC metric including:
   (a) example 1 with sigmoidal functional coefficients, and
   (b) example 2 with exponential functional coefficients (see Section 4 of main paper).
8. Run times of the local linear estimation for the MXFAR model.
9. Example codes for conducting simulation study including;
   (a) simulating data from an MXFAR process,
   (b) model selection based on the APE criterion, and
   (c) testing for nonlinearity based on the proposed nonparametric test.
