
#include <R.h>
#include <Rinternals.h>

SEXP Cindicies_(SEXP ind, SEXP starts, SEXP len) {

  int n = length(ind);
  int n_groups = length(starts);

  SEXP out = PROTECT(allocVector(VECSXP, n_groups));

  int prot_count = 1;

  int* p_len = INTEGER(len);
  int k = 0;
  int* p_ind = INTEGER(ind);
  for (int j = 0; j < n_groups; ++j) {
    const SEXP group_vec = PROTECT(allocVector(INTSXP, p_len[j]));
    int* p_group_vec = INTEGER(group_vec);
    for (int i = 0; i < p_len[j]; ++i) {
      p_group_vec[i] = p_ind[k];
      ++k;
    }
    SET_VECTOR_ELT(out, j, group_vec);
    UNPROTECT(1L);
  }

  UNPROTECT(1L);

  return out;

}
