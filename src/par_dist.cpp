#include <Rcpp.h>
using namespace Rcpp;

//' Parallel distance calculations.
//'
//' @param cols A range of column indexes.
//' @param mat A logical matrix (split into lists).
//' @export
// [[Rcpp::export]]
List par_distC(IntegerVector cols, List mat) {

  IntegerVector range_mat = Range(cols[0]-1, mat.size()-1);
  mat = mat[range_mat];

  int len_m = mat.size();
  int len_c = cols.size();

  List out(len_c);
  
  IntegerVector js = Range(0, len_m-1);
  IntegerVector outj(js.size());

  for (int i = 0; i < len_c; i++) {
    if (i % 1000 == 0) Rcpp::checkUserInterrupt();

    LogicalVector y = as<LogicalVector>(mat[i]);
    
    int k = 0;

    for (int j = i; j < len_m; j++) {
      LogicalVector matj = mat[j];
      LogicalVector ij = matj & y;
      outj[k] = sum(ij);

      k++;
    }

    out[i] = outj;
    
    js.erase(0);
    outj.erase(0);
  }

  return out;
}

// [[Rcpp::interfaces(r, cpp)]]


//' Extracts the lower triangular of a logical distance matrix.
//'
//' @param m A logical matrix.
//' @export
// [[Rcpp::export]]
LogicalVector to_lgldistC(LogicalMatrix m) {
  
  int nrow = m.nrow();
  
  LogicalVector vec(nrow * (nrow - 1) / 2);
  
  int k = 0;
  
  for (int j = 0; j < (nrow - 1); j++) {
    for (int i = j+1; i < nrow; i++) {
      vec[k++] = m(i, j);
    }
  }
  
  return vec;
}
// [[Rcpp::interfaces(r, cpp)]]

