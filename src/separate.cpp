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

std::map<grwat::basefilter, std::string> baseflow_strings = {
  {grwat::MAXWELL, "maxwell"},
  {grwat::BOUGHTON, "boughton"},
  {grwat::JAKEMAN, "jakeman"},
  {grwat::LYNE, "lynehollick"},
  {grwat::CHAPMAN, "chapman"},
  {grwat::FUREY, "furey"},
  {grwat::KUDELIN, "kudelin"}
};

std::vector<grwat::parameters> set_params(List rparams) {
  
  std::vector<grwat::parameters> cparams;
  
  for (unsigned i = 0; i < rparams.size(); ++i) {
      
    List rpar = rparams[i];
    grwat::parameters cpar;
    
    cpar.mome = rpar["winmon"];
    cpar.grad = rpar["grad1"];
    cpar.grad1 = rpar["grad2"];
    cpar.kdQgr1 = rpar["gratio"];
    cpar.polmon1 = rpar["spmon1"];
    cpar.polmon2 = rpar["spmon2"];
    cpar.polkol1 = rpar["sprisedays1"];
    cpar.polkol2 = rpar["sprisedays2"];
    cpar.polkol3 = rpar["spdays"];
    cpar.polgrad1 = rpar["sprise"];
    cpar.polgrad2 = rpar["spratio"];
    cpar.prodspada = rpar["sprecdays"];
    cpar.polcomp = rpar["spcomp"];
    cpar.nPav = rpar["precdays"];
    cpar.nZam = rpar["frostdays"];
    cpar.nWin = rpar["windays"];
    cpar.Pcr = rpar["floodprec"];
    cpar.Tcr1 = rpar["floodtemp"];
    cpar.Tcr2 = rpar["snowtemp"];
    cpar.Tzam = rpar["frosttemp"];
    cpar.Twin = rpar["wintemp"];
    cpar.SignDelta = rpar["signratio1"];
    cpar.SignDelta1 = rpar["signratio2"];
    cpar.PavRate = rpar["floodratio"];
    cpar.FlagGaps = NA_REAL;
    cpar.InterpolStep = rpar["gaplen"];
    cpar.gradabs = rpar["gradabs"];
    cpar.ModeMountain = rpar["mntmode"];
    cpar.pgrad = rpar["mntgrad"];
    cpar.polkolMount1 = rpar["mntavgdays"];
    cpar.polkolMount2 = rpar["mntratiodays"];
    cpar.polgradMount = rpar["mntratio"];
    cpar.niter = rpar["niter"];
    cpar.a = rpar["a"];
    cpar.k = rpar["k"];
    cpar.C = rpar["C"];
    cpar.aq = rpar["aq"];
    cpar.padding = rpar["padding"];
    cpar.passes = rpar["passes"];
    cpar.filter = baseflow_methods[rpar["filter"]];
    
    cparams.push_back(cpar);
  }
  
  return cparams;
}

std::vector<std::string> parnames = { 
  "winmon",
  "grad1",
  "grad2",
  "gratio",
  "spmon1",
  "spmon2",
  "sprisedays1",
  "sprisedays2",
  "spdays",
  "sprise",
  "spratio",
  "sprecdays",
  "spcomp",
  "precdays",
  "frostdays",
  "windays",
  "floodprec",
  "floodtemp",
  "frosttemp",
  "wintemp",
  "signratio1",
  "signratio2",
  "floodratio",
  "gaplen",
  "snowtemp",
  "gradabs",
  "mntmode",
  "mntgrad",
  "mntavgdays",
  "mntratiodays",
  "mntratio",
  "niter",
  "a",
  "k",
  "C",
  "aq",
  "padding",
  "passes",
  "filter"
};

List get_params(grwat::parameters p) {
  
  List params(parnames.size());
  params.attr("names") = Rcpp::wrap(parnames);
  
  params["winmon"] = double(p.mome);
  params["grad1"] = double(p.grad);
  params["grad2"] = double(p.grad1);
  params["gratio"] = double(p.kdQgr1);
  params["spmon1"] = double(p.polmon1);
  params["spmon2"] = double(p.polmon2);
  params["sprisedays1"] = double(p.polkol1);
  params["sprisedays2"] = double(p.polkol2);
  params["spdays"] = double(p.polkol3);
  params["sprise"] = double(p.polgrad1);
  params["spratio"] = double(p.polgrad2);
  params["sprecdays"] = double(p.prodspada);
  params["spcomp"] = double(p.polcomp);
  params["precdays"] = double(p.nPav);
  params["frostdays"] = double(p.nZam);
  params["windays"] = double(p.nWin);
  params["floodprec"] = double(p.Pcr);
  params["floodtemp"] = double(p.Tcr1);
  params["snowtemp"] = double(p.Tcr2);
  params["frosttemp"] = double(p.Tzam);
  params["wintemp"] = double(p.Twin);
  params["signratio1"] = double(p.SignDelta);
  params["signratio2"] = double(p.SignDelta1);
  params["floodratio"] = double(p.PavRate);
  params["gaplen"] = double(p.InterpolStep);
  params["gradabs"] = double(p.gradabs);
  params["mntmode"] = double(p.ModeMountain);
  params["mntgrad"] = double(p.pgrad);
  params["mntavgdays"] = double(p.polkolMount1);
  params["mntratiodays"] = double(p.polkolMount2);
  params["mntratio"] = double(p.polgradMount);
  params["niter"] = double(p.niter);
  params["a"] = double(p.a);
  params["k"] = double(p.k);
  params["C"] = double(p.C);
  params["aq"] = double(p.aq);
  params["padding"] = double(p.padding);
  params["passes"] = double(p.passes);
  params["filter"] = baseflow_strings[p.filter];
  return params;
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
    default:
        return Qin;
  }
}

// [[Rcpp::export]]
DataFrame separate_cpp(const std::vector<int> &Year, const std::vector<int> &Mon, const std::vector<int> &Day,
                       const std::vector<double> &Qin, const std::vector<double> &Tin, const std::vector<double> &Pin, List params, 
                       bool debug = false) {
  
  auto n = Qin.size();
  std::vector<double> Qbase(n, 0);
  std::vector<double> Quick(n, 0);
  std::vector<double> Qspri(n, 0);
  std::vector<double> Qrain(n, 0);
  std::vector<double> Qthaw(n, 0);
  std::vector<int> Type(n, 0);
  std::vector<int> Hyear(n, 0);
  std::vector<int> Jittered;
  std::vector<grwat::parameters> params_out;
  
  auto params_in = set_params(params);
  
  grwat::separate(Year, Mon, Day, Qin, Tin, Pin, Qbase, Quick, Qspri, Qrain, Qthaw, Type, Hyear, Jittered, params_in, params_out, debug);
  
  DataFrame df = DataFrame::create(Named("Qbase") = Qbase,
                                   Named("Quick") = Quick,
                                   Named("Qspri") = Qspri,
                                   Named("Qrain") = Qrain, 
                                   Named("Qthaw") = Qthaw,
                                   Named("Type") = Type,
                                   Named("Year") = Hyear);
  
  if (debug) {
    NumericVector jitattr =  wrap(Jittered);
    List parattr(params_out.size());
    
    for (unsigned i = 0; i < params_out.size(); ++i) {
      parattr[i] = get_params(params_out[i]);
    }
    
    df.attr("jittered") = jitattr;
    df.attr("params") = parattr;
  }
  
  return df;
}

