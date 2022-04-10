#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;


arma::vec dexpectilizeVec(const arma::vec& yvec, arma::vec& aweight, const arma::vec& panSizeVec) {
  
  int nsubj = panSizeVec.n_elem;
  int a = 0;
  int b = panSizeVec[0] - 1;
  
  arma::vec ydexpect(yvec.n_elem);
  
  for (int i = 0; i < nsubj; i++){
    
    arma::vec irangepanel = linspace<vec>(a, b);
    int mobs = irangepanel.n_elem;
    
    arma::colvec iypanel = yvec(span(a, b));
    arma::colvec iaweight = aweight(span(a, b));
    double sum_iaweight = sum(iaweight);
    double iexpectile = sum(iypanel%iaweight)/sum_iaweight;
    
    for(int j = 0; j < mobs; j++){
      
      int k = irangepanel[j];
      
      ydexpect[k] = yvec[k] - iexpectile;
    }
    
    a += panSizeVec[i];
    b += panSizeVec[i+1];
    
  }
  
  return ydexpect;
  
}


arma::mat dexpectilizeMat(const arma::mat& ymat, arma::vec& aweight, const arma::vec& panSizeVec) {
  
  int nsubj = panSizeVec.n_elem, nrow = ymat.n_rows, ncol = ymat.n_cols;
  
  arma::mat ydexpectMat(nrow, ncol);
  
  for(int k = 0; k < ncol; k++){
    
    int a = 0;
    int b = panSizeVec[0] - 1;
    
    arma::colvec yvec = ymat.col(k);
    
    arma::colvec ydexpect(nrow);
    
    for (int i = 0; i < nsubj; i++){
      
      arma::vec irangepanel = linspace<vec>(a, b);
      
      int mobs = irangepanel.n_elem;
      
      arma::colvec iypanel = ymat(span(a, b), k);
      
      arma::colvec iaweight = aweight(span(a, b));
      
      double sum_iaweight = sum(iaweight);
      
      double iexpectile = sum(iypanel%iaweight)/sum_iaweight;
      
      for(int j = 0; j < mobs; j++){
        
        int l = irangepanel[j];
        
        ydexpectMat(l, k) = ymat(l, k) - iexpectile;
      }
      
      a += panSizeVec[i];
      b += panSizeVec[i+1];
      
    }
    
    
  }
  
  return ydexpectMat;
  
}




arma::vec erfeRcppVec(const arma::mat& xmat, const arma::vec& yvec, const arma::vec& panSizeVec, const double asym){
  
  int ncol = xmat.n_cols; 
  
  arma::colvec betaEstimate(ncol);
  
  int it = 1;
  
  int dw1 = 1;
  
  arma::colvec w1 = 0 * yvec + 0.5;
  
  while (dw1 != 0 && it < 50) {
    
    arma::vec ydexpect = dexpectilizeVec(yvec, w1, panSizeVec);
    
    arma::mat xdexpect = dexpectilizeMat(xmat, w1, panSizeVec);
    
    betaEstimate = arma::solve(diagmat(sqrt(w1))*xdexpect, diagmat(sqrt(w1))*ydexpect); 
    
    arma::colvec z1 = xdexpect*betaEstimate;
    
    arma::colvec w01 = w1;
    
    w1.fill(asym);
    
    w1.elem(find(ydexpect <= z1)) += (1 - asym);
    
    arma::uvec difw1 = find(w1 != w01);
    
    dw1 = difw1.n_elem; 
    
    ++it;
  }
  
  return(betaEstimate);
}


arma::mat erfeRcppMat(const arma::mat& xmat, const arma::vec& yvec, const arma::vec& panSizeVec, const arma::vec& asymvec){
  
  int ncol = xmat.n_cols, nasym = asymvec.n_elem; 
  
  arma::mat betaEstimateMat(ncol, nasym);
  
  for(int k = 0; k < nasym; k++){
    
    arma::colvec betaEstimate(ncol);
    
    int it = 1;
    
    int dw1 = 1;
    
    arma::colvec w1 = 0 * yvec + 0.5;
    
    while (dw1 != 0 && it < 50) {
      
      arma::vec ydexpect = dexpectilizeVec(yvec, w1, panSizeVec);
      
      arma::mat xdexpect = dexpectilizeMat(xmat, w1, panSizeVec);
      
      betaEstimate = arma::solve(diagmat(sqrt(w1))*xdexpect, diagmat(sqrt(w1))*ydexpect); 
      
      arma::colvec z1 = xdexpect*betaEstimate;
      
      arma::colvec w01 = w1;
      
      w1.fill(asymvec[k]);
      
      w1.elem(find(ydexpect <= z1)) += (1 - asymvec[k]);
      
      arma::uvec difw1 = find(w1 != w01);
      
      dw1 = difw1.n_elem; 
      
      ++it;
    }
    
    betaEstimateMat.col(k) = betaEstimate;
    
  }
  
  
  
  return(betaEstimateMat);
}
