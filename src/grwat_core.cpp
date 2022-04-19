#include <vector>
#include <map>
#include <random>
#include <algorithm>
#include <functional>
using namespace std;

namespace grwat {

    enum basefilter {
        MAXWELL = 1,
        BOUGHTON = 2,
        JAKEMAN = 3,
        LYNE = 4,
        CHAPMAN = 5,
        FUREY = 6,
        KUDELIN = 7
    };

    struct parameters {
        int mome = 11;
        double grad = 1.0;
        double grad1 = 13.0;
        double kdQgr1 = 1500.0;
        int polmon1 = 1;
        int polmon2 = 5;
        int polkol1 = 5;
        int polkol2 = 15;
        int polkol3 = 25;
        double polgrad1 = 10;
        double polgrad2 = 6;
        unsigned prodspada = 20;
        double polcomp = 2.0;
        int nPav = 5;
        int nZam = 5;
        int nWin = 5;
        double Pcr = 0.2;
        double Tcr1 = 0.0;
        double Tcr2 = 2.0;
        double Tzam = -5.0;
        double Twin = -1.0;
        double SignDelta = 0.01;
        double SignDelta1 = 0.0015;
        double PavRate = 0.001;
        double FlagGaps = NAN;
        int InterpolStep = 15;
        double gradabs = 1000.0;
        bool ModeMountain = false;
        double pgrad = 2.0;
        int polkolMount1 = 30;
        int polkolMount2 = 30;
        double polgradMount = 1.5;
        int niter = 100;
        double a = 0.925;
        double k = 0.975;
        double C = 0.05;
        double aq = -0.5;
        double padding = 0;
        double passes = 3;
        basefilter filter = LYNE;
    };

    static bool is_singlepass(const basefilter& flt) {
        return (flt == MAXWELL) or (flt == BOUGHTON) or (flt == JAKEMAN);
    }

    static double baseflow_lyne(const double& Qfi_1,
                                const double& Qi,
                                const double& Qi_1,
                                const double& alpha = 0.925) {
        return Qfi_1 * alpha + 0.5 * (Qi - Qi_1) * (1 + alpha);
    }

    static double baseflow_chapman(const double& Qfi_1,
                                   const double& Qi,
                                   const double& Qi_1,
                                   const double& alpha = 0.925) {
        return Qfi_1 * (3 * alpha - 1) / (3 - alpha) + 2 * (Qi - alpha * Qi_1) / (3 - alpha);
    }

    static double baseflow_maxwell(const double& Qbi_1,
                                   const double& Qi,
                                   const double& Qi_1,
                                   const double& k = 0.925,
                                   const double& C = 1,
                                   const double& alpha = 1) {
        return Qbi_1 * k / (2 - k) + Qi * (1 - k) / (2 - k);
    }

    static double baseflow_boughton(const double& Qbi_1,
                                    const double& Qi,
                                    const double& Qi_1,
                                    const double& k = 0.925,
                                    const double& C = 1,
                                    const double& alpha = 1) {
        return Qbi_1 * k / (1 + C) + Qi * C / (1 + C);
    }

    static double baseflow_jakeman(const double& Qbi_1,
                                   const double& Qi,
                                   const double& Qi_1,
                                   const double& k = 0.925,
                                   const double& C = 1,
                                   const double& alpha = 1) {
        return Qbi_1 * k / (1 + C) + (Qi + alpha * Qi_1) * C / (1 + C);
    }

    static vector<double> pad_vector(const std::vector<double>::iterator& p1,
                                     const std::vector<double>::iterator& p2,
                                     const int& padding) {
        vector<double> Q(p1, p2); {
            Q.insert(Q.begin(), Q.begin() + 1, Q.begin() + padding + 1);
            Q.insert(Q.end(), Q.end() - padding - 1, Q.end() - 1);
            std::reverse(Q.begin(), Q.begin() + padding);
            std::reverse(Q.end() - padding, Q.end());
        }
        return Q;
    }


    static vector<double> get_baseflow_singlepass(const vector<double>& Qin,
                                                 const double& k = 0.975,
                                                 const double& C = 0.05,
                                                 const double& aq = -0.5,
                                                 const int& padding = 30,
                                                 basefilter method = MAXWELL) {
        map<basefilter, std::function<double(
                const double&,
                const double&,
                const double&,
                const double&,
                const double&,
                const double&)>> baseflow_singlepass = {
            {MAXWELL, baseflow_maxwell},
            {BOUGHTON, baseflow_boughton},
            {JAKEMAN, baseflow_jakeman}
        };

        std::vector<double> baseflow(Qin);

        auto p1 = baseflow.begin();
        auto p2 = baseflow.begin();

        auto is_nan = [](double d){ return isnan(d); };

        while (true) {
            p1 = std::find_if_not(p2, end(baseflow), is_nan);
            p2 = std::find_if(p1, end(baseflow), is_nan);

            if (p1 != p2) {

                auto Q = pad_vector(p1, p2, padding);
                int n = Q.size();

                auto Qb = vector<double>(n, 0);
                Qb[0] = Q[0];

                for (auto i = 1; i < n; i += 1) {
                    Qb[i] = baseflow_singlepass[method](Qb[i-1], Q[i], Q[i-1], k, C, aq);
                    if (Qb[i] > Q[i])
                        Qb[i] = Q[i];
                }

                std::copy(Qb.begin() + padding, Qb.end() - padding, p1);

            } else {
                break;
            }
        }

        return baseflow;
    }

    static vector<double> get_baseflow_recursive(const vector<double>& Qin,
                                       const double& a = 0.925,
                                       const int& padding = 30,
                                       const int& passes = 3,
                                       basefilter method = LYNE) {

        map<basefilter, std::function<double(
                const double&,
                const double&,
                const double&,
                const double&)>> baseflow_recursive = {
                {CHAPMAN, baseflow_chapman},
                {LYNE, baseflow_lyne}
        };

        std::vector<double> baseflow(Qin);

        auto p1 = baseflow.begin();
        auto p2 = baseflow.begin();

        auto is_nan = [](double d){ return isnan(d); };

        while (true) {
            p1 = std::find_if_not(p2, end(baseflow), is_nan);
            p2 = std::find_if(p1, end(baseflow), is_nan);

            if (p1 != p2) {

                auto Q = pad_vector(p1, p2, padding);

                auto n = Q.size();
                vector<double> Qf(n, 0);    // quick flow

                int delta = 1;
                int begin = 0;
                int end = n-1;
                int pass = 0;

                auto inside = [&](int index) {
                    return (delta > 0) ? index <= end : index >= end;
                };

                while (pass < passes) {
                    Qf[begin] = Q[begin];
                    auto Qbase = vector<double>(n, 0);
                    for (auto i = begin + delta; inside(i); i += delta) {
                        Qf[i] = baseflow_recursive[method](Qf[i-delta], Q[i], Q[i-delta], a);
                        Qbase[i] = (Qf[i] > 0) ? Q[i] - Qf[i] : Q[i];
                    }
                    std::swap(begin, end);
                    delta = -delta;
                    Q = Qbase;
                    pass++;
                }

                std::copy(Q.begin() + padding, Q.end() - padding, p1);

            } else {
                break;
            }
        }

        return baseflow;
    }


    static vector<double> get_baseflow_kudelin(const vector<double>& Qin,
                                               const unsigned& nmax = 0,
                                               const bool& linear = true) {
        auto n = Qin.size();
        vector<double> Qb(n, 0);

        if (linear or nmax <= 0 or nmax >= n-1) {
            auto afunc = (Qin[n-1] - Qin[0]) / (n - 1);

            for (unsigned x = 0; x < n; ++x) {
                Qb[x] = Qin[0] + x * afunc;
            }
        } else {

            auto afunc = -Qin[0] / (nmax-1);

            for (unsigned x = 0; x < nmax; ++x) {
                Qb[x] = Qin[0] + x * afunc;
            }

            afunc = Qin[n-1] / (n-nmax-1);

            for (unsigned x = nmax; x < n; ++x) {
                Qb[x] = (x-nmax) * afunc;
            }
        }

        return Qb;
    }

    static void jitter_parameters(parameters &p,
                                  const parameters &par,
                                  const vector<int> &Sumdonep) {

        // jitter parameters depending on what particular donep does not work effectively
        bool vsebylo = (Sumdonep[0] > 0 and Sumdonep[1] > 0 and Sumdonep[2] > 0) ? 1 : 0;

        auto Smallestdonep = min({Sumdonep[0], Sumdonep[1], Sumdonep[2]});

        random_device rdev;
        default_random_engine seed(rdev());
        uniform_real_distribution<> ran(0, 1);

        if (Sumdonep[0] == 0 or (vsebylo == 1 and Sumdonep[0] == Smallestdonep)) {
            auto choose = ran(seed);
            if (choose <= 0.5)
                p.polgrad1 = p.polgrad1 - 0.1 * p.polgrad1;
            else if (p.polkol1 > 2)
                p.polkol1 = p.polkol1 - 1;
            else
                p.polgrad1 = p.polgrad1 - 0.1 * p.polgrad1;

        }

        if (Sumdonep[1] == 0 or (vsebylo == 1 and Sumdonep[1] == Smallestdonep)) {
            p.polkol2 = p.polkol2 - 1;
        }

        if (Sumdonep[2] == 0 or (vsebylo == 1 and Sumdonep[2] == Smallestdonep)) {
            auto choose = ran(seed);
            if (p.ModeMountain) {
                if (choose <= 0.2) {
                    p.polkolMount1 = p.polkolMount1 - 5;
                } else if (choose > 0.2 and choose <= 0.6) {
                    if (p.polkolMount2 > 1)
                        p.polkolMount1 = p.polkolMount1 - 1;
                    else
                        p.polgradMount = p.polgradMount - 0.1 * p.polgradMount;
                } else {
                    p.polgradMount = p.polgradMount - 0.1 * p.polgradMount;
                }
            } else {
                if (choose <= 0.5) {
                    p.polgrad2 = p.polgrad2 - 0.1 * p.polgrad2;
                } else if (p.polkol3 > par.polkol3 / 2) {
                    p.polkol3 = p.polkol3 - 5;
                } else {
                    p.polgrad2 = p.polgrad2 - 0.1 * p.polgrad2;
                }
            }
        }
    }

    static vector<pair<int, int>> year_limits(const vector<int>& Year) {
        vector<pair<int, int>> limits;
        int begin = 0;
        int year = Year[0];
        for (unsigned i = 0; i < Year.size(); ++i) {
            if (Year[i] != year) {
                limits.emplace_back(pair<int, int>(begin, i-1));
                year = Year[i];
                begin = i;
            }
        }
        limits.emplace_back(pair<int, int>(begin, Year.size()-1)); // last year
        return limits;
    }

    static void fill_nodata(vector<double>& Qbase, vector<double>& Quick, vector<double>& Qspri, vector<double>& Qrain,
                            vector<double>& Qthaw, vector<int>& Type, vector<int>& Hyear,
                            unsigned start, unsigned end) {
        std::fill(Qbase.begin() + start, Qbase.begin() + end, -1);
        std::fill(Quick.begin() + start, Quick.begin() + end, -1);
        std::fill(Qspri.begin() + start, Qspri.begin() + end, -1);
        std::fill(Qrain.begin() + start, Qrain.begin() + end, -1);
        std::fill(Qthaw.begin() + start, Qthaw.begin() + end, -1);
        std::fill(Type.begin() + start, Type.begin() + end, -1);
        std::fill(Hyear.begin() + start, Hyear.begin() + end, -1);
    }

    static bool separate(const vector<int> &Year, const vector<int> &Mon, const vector<int> &Day, const vector<double> &Qin,
                         const vector<double> &Tin, const vector<double> &Pin, vector<double> &Qbase, vector<double> &Quick,
                         vector<double> &Qspri, vector<double> &Qrain, vector<double> &Qthaw, vector<int> &Type,
                         vector<int> &Hyear, vector<int> &Jittered, const vector<parameters> &params,
                         vector<parameters> &params_out, bool debug = false) {

        for (unsigned i = 0; i < Day.size(); i++) {
            if (Day[i] > 31 or (Day[i] > 29 and Mon[i] == 28))
                return false;
        }

        // WATER-RESOURCE YEARS

        auto years = year_limits(Year);
        auto nyears = years.size();
        auto ndays = Qin.size();

        auto nparams = params.size();
        if (nparams < 1 or (nparams > 1 and nparams != nyears))
            return false;

        vector<unsigned> iy(nyears, -99); // indices of water resource years starts
        vector<int> donep(3, -1); // three criteria of seasonal discharge beginning
        vector<int> sumdonep(3, 0); // sum that is used to assess the effectiveness of each donep in jittering
        vector<bool> YGaps(nyears, false); // flags if there are gaps in the year
        vector<unsigned> NumGapsY(nyears, 0); // number of gaps in the year

        auto ng = 0; // number of the year
        double polQsum = 0.0;
        double dQ = 0.0;
        auto proceed = true;

        bool separated = false;
        auto jittered = false;

        auto single = nparams == 1;

        for (unsigned i = 0; i < years.size(); ++i) {

            auto year = years[i];
            separated = false;
            jittered = false;

            auto par_new = single ? params[0] : params[i];

            iy[ng] = year.first; // 1st day of hydrograph by default!

            for (auto iter = 0; iter < par_new.niter; ++iter) {
                sumdonep = {0, 0, 0};

                for (auto l = year.first; l <= year.second; ++l) { // 177

                    donep = {-1, -1, -1};

                    if (Mon[l] >= par_new.polmon1 and Mon[l] <= par_new.polmon2 and !isnan(Qin[l])) { //223
                        dQ = 0.0;
                        proceed = true;
                        for (auto ff = 1; ff <= par_new.polkol1; ff++) {
                            if (isnan(Qin[l + ff]) or isnan(Qin[l + ff - 1])) { // goto 8787
                                proceed = false;
                                break;
                            } else {
                                dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * par_new.polkol1);
                            }
                        }

                        if (proceed) {
                            if (dQ <= par_new.polgrad1) {
                                donep[0] = -1;
                            } else {
                                donep[0] = 1;
                                sumdonep[0]++;
                            }

                            dQ = 0.0;
                            for (auto ff = 1; ff <= par_new.polkol2; ff++) {
                                dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * par_new.polkol2);
                            }

                            if (dQ <= 0) {
                                donep[1] = -1;
                            } else {
                                donep[1] = 1;
                                sumdonep[1]++;
                            }

                            if (par_new.ModeMountain) {
                                donep[2] = 1;
                                for (auto ff = 1; ff <= par_new.polkolMount1; ff++) {
                                    polQsum = 0.0;
                                    for (auto fff = ff; fff <= par_new.polkolMount2; fff++)
                                        polQsum = polQsum + Qin[l + fff];
                                    if (polQsum / (Qin[l] * par_new.polkolMount2) < par_new.polgradMount)
                                        donep[2] = -1;
                                }

                                if (donep[2] == 1)
                                    sumdonep[2]++;

                            } else {
                                polQsum = 0.0;
                                for (auto ff = 1; ff <= par_new.polkol3; ff++)
                                    polQsum = polQsum + Qin[l + ff - 1];

                                if (polQsum / (Qin[l] * par_new.polkol3) < par_new.polgrad2)
                                    donep[2] = -1;
                                else {
                                    donep[2] = 1;
                                    sumdonep[2]++;
                                }
                            }
                        }

                        if (donep[0] == 1 and donep[1] == 1 and donep[2] == 1) {
                            iy[ng] = l;
                            separated = true;
                            break;
                        }
                    }
                }

                if (separated) {
//                    if (jittered)
                    break;
                } else {
                    if (!jittered) {
                        jittered = true;
                        Jittered.push_back(Year[year.first]);
                    }
                    jitter_parameters(par_new, params[i], sumdonep);
                }

            }

            if (debug)
                params_out.push_back(par_new);

            ng++; // number of years
        }

        for (unsigned i = 0; i < iy.size(); ++i) {
            auto ilast = (i == iy.size()-1) ? Qin.size()-1 : iy[i + 1];

            NumGapsY[i] = count_if(next(Qin.begin(), iy[i]),
                                            next(Qin.begin(), ilast),
                                            [](auto q){ return isnan(q); });
            YGaps[i] = NumGapsY[i] > 0;
        }

        auto ny = iy.size();
        for (unsigned i = 0; i < ny; i++) {
            auto idx1 = iy[i];
            auto idx2 = i < ny-1 ? iy[i+1] : ndays;
            std::fill(Hyear.begin() + idx1, Hyear.begin() + idx2, Year[idx1]);
        }

        std::vector<double> deltaQ(ndays, 0);
        std::vector<double> gradQ(ndays, 0);
        std::vector<unsigned> polend(nyears, 0);

        // event flags
        std::vector<int> Psums(ndays, 0);  // is flood
        std::vector<int> Tsrs(ndays, 0);  // is flood
        std::vector<bool> FlagsPcr(ndays, false);  // is flood
        std::vector<bool> FlagsPlusTemp(ndays, false); // is thaw
        std::vector<bool> FlagsMinusTemp(ndays, false); // is thaw
        std::vector<unsigned> FactPcr(nyears, 0); // number of flood days
        std::vector<unsigned> FactPlusTemp(nyears, 0); // number of thaw days
        std::vector<unsigned> FactMinusTemp(nyears, 0); // number of thaw days
        std::vector<unsigned> startPol(nyears, 0); // number of seasonal flood begin day

        std::vector<unsigned> SummerEnd(nyears-1);
        for (unsigned i = 0; i < nyears-1; ++i)
            SummerEnd[i] = iy[i+1];
        SummerEnd.push_back(ndays-1);

        std::fill(Qbase.begin(), Qbase.end(), -1);
        fill_nodata(Qbase, Quick, Qspri, Qrain, Qthaw, Type, Hyear, 0, iy[0]);

        unsigned LocMax1;
        unsigned Flex1;
        unsigned Bend1;
        unsigned Flex2;
        unsigned Bend2;
        double Qo;

        double Psumi, Tsri;

        double dQabs = 0.0, /*dQgr = 0.0,*/ dQgr1 = 0.0, dQgr2 = 0.0, dQgr2abs = 0.0, Qgrlast = 0.0, Qgrlast1 = 0;
        unsigned nlast = 0;

        for (unsigned i = 0; i < nyears; ++i) { // main cycle along water-resource years

            auto start = iy[i]; //(i > 0) ? iy[i] : 0;
            auto end = (i < nyears-1) ? iy[i+1] : ndays-1;

            auto par = single ? params[0] : params[i];

            if (YGaps[i] or Mon[start] > par.polmon2) {
                fill_nodata(Qbase, Quick, Qspri, Qrain, Qthaw, Type, Hyear, start, end);
                continue;
            }

            int HalfSt = 0.5 * (par.nPav - 1);
            int HalfStZ = 0.5 * (par.nZam - 1);

            // position of the maximum discharge inside year
            auto nmax = start + distance(Qin.begin() + start, max_element(Qin.begin() + start, Qin.begin() + start + 2*par.polcomp*par.prodspada));

            int ngrpor = 0;

            // GROUNDWATER DISCHARGE

            for (unsigned n = start; n < end; ++n) {
                deltaQ[n] = Qin[n+1] - Qin[n];
                gradQ[n] = 100 * deltaQ[n] / Qin[n];

                if (n == start or n == end-1) { // ground in first and last is equal to Qin
                    Qbase[n] = Qin[n];
                    Qgrlast1 = Qin[n];
                    Qgrlast = Qin[n];
                    nlast = n;
                } else {

                    dQ = 100 * abs(Qin[n - 1] - Qin[n]) / Qin[n - 1];
                    dQabs = abs(Qin[n - 1] - Qin[n]);
                    dQgr1 = 100 * abs(Qgrlast1 - Qin[n]) / Qgrlast1;
                    dQgr2 = 100 * abs(Qgrlast - Qin[n]) / ((n - nlast + 1) * Qgrlast);
                    dQgr2abs = abs(Qgrlast - Qin[n]) / (n - nlast + 1);

                    if (Qin[n] > Qgrlast or n > (nlast + 20)) {
                        auto con1 = ((n - nmax) > par.prodspada) and
                                    (dQ > par.grad or dQgr1 > par.kdQgr1 or dQgr2 > par.grad or
                                     dQabs > par.gradabs or dQgr2abs > par.gradabs);
                        auto con2 = dQ > par.grad1 or dQgr1 > par.kdQgr1 or dQgr2 > par.grad1  or
                                    dQabs > par.gradabs or dQgr2abs > par.gradabs;
                        if (con1 or con2)
                            continue;
                    } else {
                        auto con1 = dQ > 20 * par.grad or dQgr1 > 20 * par.kdQgr1 or dQgr2 > 20 * par.grad;
                        auto con2 = abs(Qin[n + 1] - Qin[n - 1]) < abs(Qin[n + 1] - Qin[n]);
                        if (con1 and con2)
                            continue;
                    }

                    Qbase[n] = Qin[n];
                    if (n > nmax) // tends to get wrong spring flood
                        ngrpor++;

                    if (ngrpor == 1) {
                        polend[i] = n;
                    }

                    Qgrlast = Qin[n];
                    nlast = n;
                }
            }

            // seasonal frechet is not found
            if (polend[i] == 0) {
                fill_nodata(Qbase, Quick, Qspri, Qrain, Qthaw, Type, Hyear, start, end);
                continue;
            }

            // 508: Smoothing of Qbase (NEW)


            for (auto k = start; k < end;) {
                if (Qbase[k] < 0) { // NEW
                    auto kk = k + 1;
                    while (kk < end) {
                        if (Qbase[kk] >= 0) {
                            auto quick = std::vector<double>(Qin.begin() + k - 1, Qin.begin() + kk + 1);
                            auto a = quick[0];
                            auto b = quick[quick.size() - 1];
                            auto nx = quick.size();
                            auto dx = b - a;
                            auto is_freshet = (kk == polend[i]);

                            auto dquick = std::vector<double>(quick.size());
                            for (unsigned x = 0; x < nx; x++) {
                                dquick[x] = quick[x] - quick[0] - dx * x / (nx-1);
                            }

                            if (par.filter == KUDELIN) {
                                auto baseflow = get_baseflow_kudelin(quick, nmax-k, !is_freshet);
                                std::copy(baseflow.begin(), baseflow.end(), Qbase.begin() + k - 1);
                            } else {
                                auto qbaseflow =
                                    is_singlepass(par.filter) ?
                                        get_baseflow_singlepass(dquick, par.k, par.C, par.aq, par.padding, par.filter) :
                                    get_baseflow_recursive(dquick, par.a, par.padding, par.passes, par.filter);

                                auto baseflow = std::vector<double>(quick.size());
                                for (unsigned x = 0; x < nx; x++) {
                                    baseflow[x] = qbaseflow[x] + quick[0] + dx * x / (nx-1);
                                }

                                std::copy(baseflow.begin(), baseflow.end(), Qbase.begin() + k - 1);
                            }

                            k = kk;
                            break;
                        }
                        kk++;
                    }
                }
                k++;
            }

            // CHECK THE CONTINUOUS POSITIVE QUICKFLOW

            auto maxstart = start;
            auto maxend = polend[i];
            double maxcum = 0;

            auto s = start;
            while (s < polend[i]) {

                if (abs(Qbase[s] - Qin[s]) <= 0.000001) {
                    s++;
                    continue;
                }

                auto cum = 0;
                auto e = s;
                while (e <= polend[i]) {
                    cum += Qin[e] - Qbase[e];
                    e++;

                    if (abs(Qbase[e] - Qin[e]) <= 0.000001)
                        break;
                }

                if (cum > maxcum) {
                    if (Mon[s] <= par.polmon2) {
                        maxcum = cum;
                        maxstart = s;
                        maxend = e;
                    } else {
                        break;
                    }
                }

                s = e;
            }

            for (unsigned k = start; k < maxstart; ++k)
                Qthaw[k] = Qin[k] - Qbase[k];

            iy[i] = maxstart;
            start = maxstart;
            polend[i] = maxend;


            // FLOODS AND THAWS SEPARATION

            // Check rain and thaws
            for (unsigned m = start + HalfSt; m < end - HalfSt; ++m) {

                Psumi = std::accumulate(Pin.begin() + m - HalfSt, Pin.begin() + m + HalfSt, 0.0);
                Tsri = std::accumulate(Tin.begin() + m - HalfSt, Tin.begin() + m + HalfSt, 0.0) / par.nPav;

                Psums[m] = Psumi;

                if (Psumi >= par.Pcr and Tsri >= par.Tcr1) { // critical rain
                    FactPcr[i]++;
                    FlagsPcr[m] = true;
                }

                Tsrs[m] = Tsri;

                if (Tsri >= par.Tcr2) { // substantial plus temp
                    FactPlusTemp[i]++;
                    FlagsPlusTemp[m] = true;
                }
            }

            // Check frosts
            for (unsigned m = start + HalfStZ; m < end - HalfStZ; ++m) {
                Tsri = std::accumulate(Tin.begin() + m - HalfStZ, Tin.begin() + m + HalfStZ, 0.0) / par.nZam;

                if (Tsri < par.Tzam) {
                    FactMinusTemp[i]++;
                    FlagsMinusTemp[m] = true;
                }
            }

            startPol[i] = start; // initial freshet starting day before thaws are checked
            LocMax1 = start;
            Flex1 = start;
            Bend1 = nmax;

            // search for upwards floods

            for (auto p = nmax-2; p > startPol[i]; --p) {
                unsigned FlexPrev = start;
                if (p < Bend1) {
                    if ((deltaQ[p] <= -Qin[nmax] * par.SignDelta) or ((deltaQ[p] + deltaQ[p-1]) <= -Qin[nmax] * par.SignDelta)) {
                        for (auto pp = p; pp < nmax-2; ++pp) {
                            if (deltaQ[pp] > 0) {
                                Flex1 = pp;
                                break;
                            }
                        }
                    }

                    if (Flex1 >= start) {
                        for (auto u = Flex1; u > startPol[i]; --u) { // 602
                            if ((deltaQ[u] <= (-Qin[nmax] * par.SignDelta * 0.5)) or ((deltaQ[u] + deltaQ[u - 1]) <= (-Qin[nmax] * par.SignDelta * 0.5))) {
                                for (auto pp = u; pp < Flex1-1; ++pp) {
                                    if (deltaQ[pp] > 0) {
                                        FlexPrev = pp;
                                    }
                                }
                            }
                        } // 611

                        if (FlexPrev > start) {
                            LocMax1 = std::distance(Qin.begin() + FlexPrev, max_element(Qin.begin() + FlexPrev, Qin.begin() + Flex1)) + FlexPrev - 1;
                        } else {
                            LocMax1 = std::distance(Qin.begin() + start, max_element(Qin.begin() + start, Qin.begin() + Flex1)) + start;
                        } // 617

                        // Frosts — backup/removed.cpp

                        // Rains
                        for (unsigned pp = LocMax1; pp > start; --pp) {
                            if ((Qin[pp] < Qin[Flex1]) and (deltaQ[pp - 1] <= ((Qin[Flex1] - Qin[pp]) / (Flex1 - pp)))) {
                                Bend1 = pp;
                                break;
                            }
                        }

                        for (auto pp = Bend1 - 2 * HalfSt; pp < LocMax1; ++pp) {

                            if (FlagsPcr[pp]) { // Rain

                                auto afunc = (Qin[Flex1] - Qin[Bend1]) / (Flex1 - Bend1);
                                auto bfunc = Qin[Flex1] - afunc * Flex1;

                                for (unsigned qq = Bend1; qq < Flex1; ++qq) {
                                    if (auto qval = afunc * qq + bfunc; qval < Qin[qq]) {
                                        Qrain[qq] = Qin[qq] - qval;
                                    }
                                }
                            }
                        }

                    }
                }
            }

            // if this peak is the first seasonal freshet

            auto nmax2 = nmax; // (plus_found or minus_found) ? nmax : LocMax1; TODO: repair true maximum
            auto nmax2_bend = polend[i];

            // DOWNWARD FLOODS

            Flex2 = start-1;
            Bend2 = start-1;

            bool floods_found = false;
            bool first_peak = true;

            bool early_polend = false;

            for (auto p = nmax2; p < polend[i] - 1; ++p) {
                if ((p > Bend2) and
                  ((deltaQ[p] >= Qin[nmax2] * par.SignDelta) or ((deltaQ[p] + deltaQ[p + 1]) >= Qin[nmax2] * par.SignDelta))) {
                    for (auto pp = p; pp >= nmax2; --pp) {
                        if (deltaQ[pp] < 0) {
                            Flex2 = pp + 1;
                            if (first_peak) {
                                nmax2_bend = Flex2;
                                first_peak = false;
                            }
                            break;
                        }
                    }

                    for (unsigned pp = Flex2 + 1; pp < polend[i]; ++pp) {
                        if (((Qin[pp] < Qin[Flex2])
                            and  (deltaQ[pp] >= (Qin[pp] - Qin[Flex2]) / (pp - Flex2)))
                            or (pp == polend[i]-1)) {
                            Bend2 = pp;

                            for (auto ppp = Bend2 - HalfSt; ppp > Flex2 - 2*HalfSt; --ppp) {
                                if (FlagsPcr[ppp]) {
                                    auto z = -log(Qin[Bend2] / Qin[Flex2]) / (Bend2 - Flex2);
                                    Qo = Qin[Flex2];
                                    for (auto qq = Flex2; qq < Bend2; ++qq) {
                                        auto qval = Qo * exp(-z * (qq-Flex2));

                                        if (qval <= Qin[qq]) {
                                            if (qval > Qbase[qq]) {
                                                Qrain[qq] = Qin[qq] - qval;
                                            } else {
                                                early_polend = true;
                                                polend[i] = qq;
                                                break;
                                            }

                                        }
                                    }

                                    if (early_polend)
                                        break;

                                    floods_found = true;

                                    p = Bend2; // to promote p cycle after the peak
                                    pp = polend[i]; // to break the pp cycle

                                    break;
                                }
                            }

                            if (!floods_found) {
                                nmax2 = Flex2 + distance(Qin.begin() + Flex2, max_element(Qin.begin() + Flex2, Qin.begin() + Bend2));
                                nmax2_bend = Bend2;
                            }

                            break;
                        }
                    }

                    if (early_polend)
                        break;
                }
            }

            // least squares freshet flood decay
            if (floods_found and not early_polend and ((polend[i] - start) >= (par.prodspada * par.polcomp))) {

                auto z = -log(Qin[nmax2_bend] / Qin[nmax2]) / (nmax2_bend - nmax2);

                Qo = Qin[nmax2];
                bool is_endpol = false;
                bool is_endflood = false;
                auto ref = nmax2;

                auto x = nmax2_bend;

                while (x < polend[i]) {

                    auto q = Qo * exp(-z * (x-ref));

                    if (is_endpol) {
                        q = Qbase[x]; // 0
                    } else if (is_endflood) {
                        q = Qin[x];
                    } else if (x > nmax2_bend) {
                        if (q < Qbase[x]) {
                            is_endpol = true;
                            polend[i] = x;
                            q = Qbase[x]; // 0
                        }

                        if (q > Qin[x]) {
                            z = -log(Qbase[x] / Qin[nmax2_bend]) / (x - nmax2_bend);
                            x = nmax2_bend;
                            ref = nmax2_bend;
                            Qo = Qin[nmax2_bend];
                            continue;
                        }
                    }
                    Qrain[x] = Qin[x] - q;
                    x++;
                }

            }

            // non-mountain separation
            auto HalfStW = (par.nWin - 1) / 2;

            auto winmin = polend[i] + 1;
            while (Mon[winmin] <= par.polmon2)
                winmin++;

            for (auto pp = winmin; pp < end; ++pp) {

                bool MarkCold = true;

                for (auto u = pp; u < pp + par.nWin; ++u) {
                    if (Tin[u] > par.Twin) {
                        MarkCold = false;
                        break;
                    }
                }

                if (MarkCold) {
                    if (Qin[pp + HalfStW] == Qbase[pp + HalfStW]) {
                      SummerEnd[i] = pp + HalfStW;
                    } else {
                      auto ppp = pp;
                      while (Qin[ppp] > Qbase[ppp]) {
                        ppp++;
                      }
                      SummerEnd[i] = ppp;
                    }
                    break;
                }
            }

            for (unsigned k = polend[i]; k < end; ++k) {
                if (Qin[k] > Qbase[k]) {
                    if (k <= SummerEnd[i]) {
                        Qrain[k] = Qin[k] - Qbase[k];
                    } else {
                        Qthaw[k] = Qin[k] - Qbase[k];
                    }
                }
            }

            for (auto k = start; k < end; ++k) {
                Qspri[k] = Qin[k] - Qbase[k] - Qthaw[k] - Qrain[k];
                Quick[k] = Qin[k] - Qbase[k];
            }

            std::fill(Type.begin() + start, Type.begin() + polend[i], 0);
            std::fill(Type.begin() + polend[i], Type.begin() + SummerEnd[i], 1);
            std::fill(Type.begin() + SummerEnd[i], Type.begin() + end, 2);

        }

        return true;
    }
}
