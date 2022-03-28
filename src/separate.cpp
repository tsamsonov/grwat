#include "grwat_core.cpp"
#include <Rcpp.h>
using namespace Rcpp;

std::map<std::string, grwat::basefilter> baseflow_methods = {
  {"maxwell", grwat::MAXWELL},
  {"boughton", grwat::BOUGHTON},
  {"jakeman", grwat::JAKEMAN},
  {"lynehollick", grwat::LYNE},
  {"chapman", grwat::CHAPMAN},
  {"furey", grwat::FUREY},
  {"kudelin", grwat::KUDELIN}
};

grwat::parameters set_params(List params) {
  grwat::parameters p;
    p.mome = params["winmon"];
    p.grad = params["grad1"];
    p.grad1 = params["grad2"];
    p.kdQgr1 = params["gratio"];
    p.polmon1 = params["ftmon1"];
    p.polmon2 = params["ftmon2"];
    p.polkol1 = params["ftrisedays1"];
    p.polkol2 = params["ftrisedays2"];
    p.polkol3 = params["ftdays"];
    p.polgrad1 = params["ftrise"];
    p.polgrad2 = params["ftratio"];
    p.prodspada = params["ftrecdays"];
    p.polcomp = params["ftcomp"];
    p.nPav = params["precdays"];
    p.nZam = params["frostdays"];
    p.nWin = params["windays"];
    p.Pcr = params["floodprec"];
    p.Tcr1 = params["floodtemp"];
    p.Tcr2 = params["snowtemp"];
    p.Tzam = params["frosttemp"];
    p.Twin = params["wintemp"];
    p.SignDelta = params["signratio1"];
    p.SignDelta1 = params["signratio2"];
    p.PavRate = params["floodratio"];
    p.FlagGaps = NA_REAL;
    p.InterpolStep = params["gaplen"];
    p.gradabs = params["gradabs"];
    p.ModeMountain = params["mntmode"];
    p.pgrad = params["mntgrad"];
    p.polkolMount1 = params["mntavgdays"];
    p.polkolMount2 = params["mntratiodays"];
    p.polgradMount = params["mntratio"];
    p.niter = params["niter"];
    p.a = params["a"];
    p.k = params["k"];
    p.C = params["C"];
    p.aq = params["aq"];
    p.padding = params["padding"];
    p.passes = params["passes"];
    p.filter = baseflow_methods[params["filter"]];
  return p;
}

// [[Rcpp::export]]
std::vector<double> get_baseflow_cpp(const std::vector<double> &Qin, 
                                     const double& a,
                                     const double& k,
                                     const double& C,
                                     const double& aq,
                                     const int& passes,
                                     const int& padding,
                                     std::string method) {
  auto b = baseflow_methods[method];
  switch (b) 
  {
    case grwat::MAXWELL:
    case grwat::BOUGHTON:
    case grwat::JAKEMAN:
      return grwat::get_baseflow_singlepass(Qin, k, C, aq, padding, b);
    case grwat::LYNE:
    case grwat::CHAPMAN:
      return grwat::get_baseflow_recursive(Qin, a, padding, passes, b);
    case grwat::FUREY:
    case grwat::KUDELIN:
        return Qin;
  }
}

// [[Rcpp::export]]
DataFrame separate_cpp(const std::vector<int> &Year, const std::vector<int> &Mon, const std::vector<int> &Day,
                       const std::vector<double> &Qin, const std::vector<double> &Tin, const std::vector<double> &Pin, List params) {
  
  auto n = Qin.size();
  std::vector<double> Qbase(n, 0);
  std::vector<double> Quick(n, 0);
  std::vector<double> Qseas(n, 0);
  std::vector<double> Qrain(n, 0);
  std::vector<double> Qthaw(n, 0);
  std::vector<double> Qpb(n, 0);
  std::vector<int> Type(n, 0);
  std::vector<int> Hyear(n, 0);
  std::vector<int> Jittered(n, 0);
  
  auto p = set_params(params);
  
  grwat::separate(Year, Mon, Day, Qin, Tin, Pin, Qbase, Quick, Qseas, Qrain, Qthaw, Qpb, Type, Hyear, Jittered, p);
  
  DataFrame df = DataFrame::create(Named("Qbase") = Qbase,
                                   Named("Quick") = Quick,
                                   Named("Qseas") = Qseas,
                                   Named("Qrain") = Qrain, 
                                   Named("Qthaw") = Qthaw,
                                   Named("Qpb") = Qpb,
                                   Named("Type") = Type,
                                   Named("Year") = Hyear,
                                   Named("Jittered") = Jittered);
  
  return df;
}

