// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// dexpectilizeVec
arma::vec dexpectilizeVec(const arma::vec& yvec, arma::vec& aweight, const arma::vec& panSizeVec);
RcppExport SEXP _erfe_dexpectilizeVec(SEXP yvecSEXP, SEXP aweightSEXP, SEXP panSizeVecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type yvec(yvecSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type aweight(aweightSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type panSizeVec(panSizeVecSEXP);
    rcpp_result_gen = Rcpp::wrap(dexpectilizeVec(yvec, aweight, panSizeVec));
    return rcpp_result_gen;
END_RCPP
}
// dexpectilizeMat
arma::mat dexpectilizeMat(const arma::mat& ymat, arma::vec& aweight, const arma::vec& panSizeVec);
RcppExport SEXP _erfe_dexpectilizeMat(SEXP ymatSEXP, SEXP aweightSEXP, SEXP panSizeVecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type ymat(ymatSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type aweight(aweightSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type panSizeVec(panSizeVecSEXP);
    rcpp_result_gen = Rcpp::wrap(dexpectilizeMat(ymat, aweight, panSizeVec));
    return rcpp_result_gen;
END_RCPP
}
// erfeRcppVec
arma::vec erfeRcppVec(const arma::mat& xmat, const arma::vec& yvec, const arma::vec& panSizeVec, const double asym);
RcppExport SEXP _erfe_erfeRcppVec(SEXP xmatSEXP, SEXP yvecSEXP, SEXP panSizeVecSEXP, SEXP asymSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type xmat(xmatSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type yvec(yvecSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type panSizeVec(panSizeVecSEXP);
    Rcpp::traits::input_parameter< const double >::type asym(asymSEXP);
    rcpp_result_gen = Rcpp::wrap(erfeRcppVec(xmat, yvec, panSizeVec, asym));
    return rcpp_result_gen;
END_RCPP
}
// erfeRcppMat
arma::mat erfeRcppMat(const arma::mat& xmat, const arma::vec& yvec, const arma::vec& panSizeVec, const arma::vec& asymvec);
RcppExport SEXP _erfe_erfeRcppMat(SEXP xmatSEXP, SEXP yvecSEXP, SEXP panSizeVecSEXP, SEXP asymvecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type xmat(xmatSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type yvec(yvecSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type panSizeVec(panSizeVecSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type asymvec(asymvecSEXP);
    rcpp_result_gen = Rcpp::wrap(erfeRcppMat(xmat, yvec, panSizeVec, asymvec));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_hello_world
arma::mat rcpparma_hello_world();
RcppExport SEXP _erfe_rcpparma_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpparma_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_outerproduct
arma::mat rcpparma_outerproduct(const arma::colvec& x);
RcppExport SEXP _erfe_rcpparma_outerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_outerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_innerproduct
double rcpparma_innerproduct(const arma::colvec& x);
RcppExport SEXP _erfe_rcpparma_innerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_innerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpparma_bothproducts
Rcpp::List rcpparma_bothproducts(const arma::colvec& x);
RcppExport SEXP _erfe_rcpparma_bothproducts(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpparma_bothproducts(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_erfe_dexpectilizeVec", (DL_FUNC) &_erfe_dexpectilizeVec, 3},
    {"_erfe_dexpectilizeMat", (DL_FUNC) &_erfe_dexpectilizeMat, 3},
    {"_erfe_erfeRcppVec", (DL_FUNC) &_erfe_erfeRcppVec, 4},
    {"_erfe_erfeRcppMat", (DL_FUNC) &_erfe_erfeRcppMat, 4},
    {"_erfe_rcpparma_hello_world", (DL_FUNC) &_erfe_rcpparma_hello_world, 0},
    {"_erfe_rcpparma_outerproduct", (DL_FUNC) &_erfe_rcpparma_outerproduct, 1},
    {"_erfe_rcpparma_innerproduct", (DL_FUNC) &_erfe_rcpparma_innerproduct, 1},
    {"_erfe_rcpparma_bothproducts", (DL_FUNC) &_erfe_rcpparma_bothproducts, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_erfe(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}