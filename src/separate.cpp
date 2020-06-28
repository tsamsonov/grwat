#include "grwat_core.cpp"
#include <Rcpp.h>
using namespace Rcpp;

grwat::parameters set_params(List params) {
  grwat::parameters p;
    p.mome = params["mome"];
    p.grad = params["grad"];
    p.grad1 = params["grad1"];
    p.kdQgr1 = params["kdQgr1"];
    p.polmon1 = params["polmon1"];
    p.polmon2 = params["polmon2"];
    p.polkol1 = params["polkol1"];
    p.polkol2 = params["polkol2"];
    p.polkol3 = params["polkol3"];
    p.polgrad1 = params["polgrad1"];
    p.polgrad2 = params["polgrad2"];
    p.prodspada = params["prodspada"];
    p.nPav = params["nPav"];
    p.nZam = params["nZam"];
    p.nWin = params["nWin"];
    p.Pcr = params["Pcr"];
    p.Tcr1 = params["Tcr1"];
    p.Tcr2 = params["Tcr2"];
    p.Tzam = params["Tzam"];
    p.Twin = params["Twin"];
    p.SignDelta = params["SignDelta"];
    p.SignDelta1 = params["SignDelta1"];
    p.PavRate = params["PavRate"];
    p.FlagGaps = params["FlagGaps"];
    p.InterpolStep = params["InterpolStep"];
    p.gradabs = params["gradabs"];
    p.ModeMountain = params["ModeMountain"];
    p.pgrad = params["pgrad"];
    p.polkolMount1 = params["polkolMount1"];
    p.polkolMount2 = params["polkolMount2"];
    p.polgradMount = params["polgradMount"];
  return p;
}

// [[Rcpp::export]]
std::vector<double> get_baseflow_cpp(const std::vector<double> &Qin, 
                                     const double& alpha,
                                     const int& padding,
                                     const int& passes,
                                     std::string method) {
  return grwat::get_baseflow(Qin, alpha, padding, passes, grwat::LYNE);
}

// [[Rcpp::export]]
DataFrame separate_cpp(const std::vector<int> &Year, const std::vector<int> &Mon, const std::vector<int> &Day,
                       const std::vector<double> &Qin, const std::vector<double> &Tin, const std::vector<double> &Pin, List params, int niter) {
  
  auto n = Qin.size();
  std::vector<double> Qgr(n, 0);
  std::vector<double> Qpol(n, 0);
  std::vector<double> Qpav(n, 0);
  std::vector<double> Qthaw(n, 0);
  std::vector<double> Qpb(n, 0);
  
  auto p = set_params(params);
  
  grwat::separate(Year, Mon, Day, Qin, Tin, Pin, Qgr, Qpol, Qpav, Qthaw, Qpb, p, niter);
  
  DataFrame df = DataFrame::create(Named("Qgr") = Qgr, 
                                   Named("Qpol") = Qpol,
                                   Named("Qpav") = Qpav, 
                                   Named("Qthaw") = Qthaw,
                                   Named("Qpb") = Qpb);
  
  return df;
}

