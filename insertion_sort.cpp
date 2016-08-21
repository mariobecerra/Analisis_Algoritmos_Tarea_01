#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector insertionsortC(NumericVector vector) {
    int n = vector.size();

    double aux;
    int i , j;

    for(i = 1; i < n; i++) {
        aux = vector[i];
        j = i - 1;
        while(j >= 0 && vector[j] > aux) {
            vector[j + 1] = vector[j];
            j = j - 1;
            }
        vector[j + 1] = aux;
        }
    return vector;
}