#include <Rcpp.h>
using namespace Rcpp;

//' Parallel distance calculations.
//'
//' @param cols A range of column indexes.
//' @param mat A logical matrix.
//' @export
// [[Rcpp::export]]
List par_distC(IntegerVector cols, List mat) {

  IntegerVector range_mat = Range(cols[0]-1, mat.size()-1);
  mat = mat[range_mat];

  int len_m = mat.size();
  int len_c = cols.size();

  List out(len_c);

  for (int i = 0; i < len_c; i++) {
    if (i % 1000 == 0) Rcpp::checkUserInterrupt();

    LogicalVector y = as<LogicalVector>(mat[i]);

    IntegerVector js = Range(i, len_m-1);
    IntegerVector outj(js.size());

    int k = 0;

    for (int j = i; j < len_m; j++) {
      LogicalVector matj = mat[j];
      LogicalVector ij = matj & y;
      outj[k] = sum(ij);

      k++;
    }

    out[i] = outj;
  }

  return out;
}


// [[Rcpp::interfaces(r, cpp)]]


