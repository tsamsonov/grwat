#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <random>
#include <algorithm>
using namespace std;

namespace grwat {
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
        int prodspada = 90;
        int nPav = 2;
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
        double FlagGaps = -999.0;
        int InterpolStep = 15;
        double gradabs = 1000.0;
        bool ModeMountain = false;
        double pgrad = 2.0;
        int polkolMount1 = 30;
        int polkolMount2 = 30;
        double polgradMount = 1.5;
    };

    enum basefilter {
        CHAPMAN,
        BOUGHTON,
        JAKEMAN,
        LYNE,
        MAU,
        FUREY
    };

    static vector<double> get_baseflow(const vector<double>& Qin,
                                       const double& alpha = 0.925,
                                       const int& padding = 30,
                                       const int& passes = 3,
                                       basefilter method = LYNE) {



        vector<double> Q(Qin.begin(), Qin.end()); {
            Q.insert(Q.begin(), Q.begin() + 1, Q.begin() + padding + 1);
            Q.insert(Q.end(), Q.end() - padding - 1, Q.end() - 1);
            std::reverse(Q.begin(), Q.begin() + padding);
            std::reverse(Q.end() - padding, Q.end());
        }

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
                Qf[i] = alpha * Qf[i-delta] + 0.5 * (1 + alpha) * (Q[i] - Q[i-delta]);
                Qbase[i] = (Qf[i] > 0) ? Q[i] - Qf[i] : Q[i];
            }
            std::swap(begin, end);
            delta = -delta;
            Q = Qbase;
            pass++;
        }

        return vector<double>(Q.begin() + padding, Q.end() - padding);
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

    static map<int, pair<int, int>> year_limits(const vector<int>& Year) {
        map<int, pair<int, int>> limits;
        int begin = 0;
        int year = Year[0];
        for (int i = 0; i < Year.size(); ++i) {
            if (Year[i] != year) {
                limits[year] = pair<int, int>(begin, i-1);
                year = Year[i];
                begin = i;
            }
        }
        return limits;
    }

    static void separate(const vector<int>& Year, const vector<int>& Mon, const vector<int>& Day,
                  const vector<double>& Qin, const vector<double>& Tin, const vector<double>& Pin,
                  vector<double>& Qgr, vector<double>& Qpol, vector<double>& Qpav,
                  vector<double>& Qthaw, vector<double>& Qpb,
                  const parameters& par, const int& niter = 100) {

        // detect gaps in data
        map<int, int> FactGapsin;
        int i = 0;
        for (auto it = Qin.begin(); it != Qin.end(); ++it) {
            if (*it == par.FlagGaps) {
                int k = 0;
                while (it != Qin.end()) {
                    if (*it != par.FlagGaps) break;
                    k++;
                    it++;
                }
                FactGapsin[i] = k;
                i += k;
            }
            i++;
        }

        // WATER-RESOURCE YEARS

        auto years = year_limits(Year);
        auto nyears = years.size();
        auto ndays = Qin.size();

        vector<int> iy(nyears, -99); // indices of water resource years starts
        vector<int> donep(3, -1); // three criteria of seasonal discharge beginning
        vector<int> sumdonep(3, 0); // sum that is used to assess the effectiveness of each donep in jittering
        vector<bool> YGaps(nyears, false); // flags if there are gaps in the year
        map<int, int> NumGapsY; // number of gaps in the year

        auto ng = 0; // number of the year
        double polQsum = 0.0;
        double dQ = 0.0;
        auto proceed = true;

        bool separated = false;
        auto jittered = false;
        grwat::parameters p = par;


        for (auto year: years) {
            separated = false;
            jittered = false;
            p = par;
            for (auto iter = 0; iter < niter; ++iter) {
                sumdonep = {0, 0, 0};

                for (auto l = year.second.first; l <= year.second.second; ++l) { // 177

                    donep = {-1, -1, -1};

                    if (Mon[l] >= p.polmon1 and Mon[l] <= p.polmon2 and Qin[l] != p.FlagGaps) { //223
                        dQ = 0.0;
                        proceed = true;
                        for (auto ff = 1; ff <= p.polkol1; ff++) {
                            if (Qin[l + ff] == p.FlagGaps or Qin[l + ff - 1] == p.FlagGaps) { // goto 8787
                                proceed = false;
                                break;
                            } else {
                                dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * p.polkol1);
                            }
                        }

                        if (proceed) {
                            if (dQ <= p.polgrad1) {
                                donep[0] = -1;
                            } else {
                                donep[0] = 1;
                                sumdonep[0]++;
                            }

                            dQ = 0.0;
                            for (auto ff = 1; ff <= p.polkol2; ff++) {
                                dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * p.polkol2);
                            }

                            if (dQ <= 0) {
                                donep[1] = -1;
                            } else {
                                donep[1] = 1;
                                sumdonep[1]++;
                            }

                            if (p.ModeMountain) {
                                donep[2] = 1;
                                for (auto ff = 1; ff <= p.polkolMount1; ff++) {
                                    polQsum = 0.0;
                                    for (auto fff = ff; fff <= p.polkolMount2; fff++)
                                        polQsum = polQsum + Qin[l + fff];
                                    if (polQsum / (Qin[l] * p.polkolMount2) < p.polgradMount)
                                        donep[2] = -1;
                                }

                                if (donep[2] == 1)
                                    sumdonep[2]++;

                            } else {
                                polQsum = 0.0;
                                for (auto ff = 1; ff <= p.polkol3; ff++)
                                    polQsum = polQsum + Qin[l + ff - 1];

                                if (polQsum / (Qin[l] * p.polkol3) < p.polgrad2)
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
                    if (jittered)
                        cout << iter << " iterations" << endl;
                    break;
                } else {
                    if (!jittered) {
                        cout << endl << "POLFINDER: " << year.first << ", ";
                        jittered = true;
                    }
                    jitter_parameters(p, par, sumdonep);
                }

            }

            NumGapsY[year.first] = count(next(Qin.begin(), year.second.first),
                                         next(Qin.begin(), year.second.second),
                                         par.FlagGaps);
            YGaps[ng] = NumGapsY[year.first] > 0;
            ng++; // number of years
        }

//        cout << endl <<  "YEAR LIMITS:" << endl;
//        for (auto y: years)
//            cout << y.first << ' ' << y.second.first << ' '  << y.second.second << endl;

        cout << endl << "YEAR BEGINNINGS:" << endl;
        int j = 1;

        for (auto i: iy) {
            Qpol[i] = 1;
            cout << Day[i] << '-' << Mon[i] << '-' << Year[i] << ' ';
            if (j % 10 == 0)
                cout << endl;
            j++;
        }

        if (j % 10 == 0)
            cout << endl;

        if (FactGapsin.size() > 0) {
            cout << endl << "MISSING VALUES:" << endl;
            for (auto g: FactGapsin) {
                cout << g.second << " values starting from " << Day[g.first] << '-' << Mon[g.first] << '-' << Year[g.first] << endl;
            }
        } else {
            cout << endl << "NO GAPS DETECTED" << endl;
        }

        std::vector<double> deltaQ(ndays, 0);
        std::vector<double> gradQ(ndays, 0);
        std::vector<int> polend(nyears, 0);

        // event flags
        std::vector<int> Psums(ndays, 0);  // is flood
        std::vector<int> Tsrs(ndays, 0);  // is flood
        std::vector<bool> FlagsPcr(ndays, false);  // is flood
        std::vector<bool> FlagsPlusTemp(ndays, false); // is thaw
        std::vector<bool> FlagsMinusTemp(ndays, false); // is thaw
        std::vector<int> FactPcr(nyears, 0); // number of flood days
        std::vector<int> FactPlusTemp(nyears, 0); // number of thaw days
        std::vector<int> FactMinusTemp(nyears, 0); // number of thaw days
        std::vector<int> startPol(nyears, 0); // number of seasonal flood begin day
        // linear interpolation of Qgr
        std::vector<double> Qy(nyears, 0);
        std::vector<double> Qygr(nyears, 0);

        int LocMax1;
        int Flex1;
        int Bend1;
        int Flex2;
        int Bend2;
        double Qo;

        int HalfSt = 0.5 * (p.nPav - 1);
        int HalfStZ = 0.5 * (p.nZam - 1);
        double Psumi, Tsri;

        double dQabs = 0.0, dQgr = 0.0, dQgr1 = 0.0, dQgr2 = 0.0, dQgr2abs = 0.0, Qgrlast = 0.0, Qgrlast1 = 0;
        int nlast = 0;

        std::cout << std::endl << "SEPARATING DISCHARGE" << std::endl;

        for (auto i = 0; i < nyears; ++i) { // main cycle along water-resource years
            auto start = (i > 0) ? iy[i] : 0;
            auto end = (i < nyears-1) ? iy[i+1] : ndays-1;
            auto ny = end - start;

            std::cout << "Year " << i + 1 << " from " << nyears << std::endl;

            // position of the maximum discharge inside year
            auto nmax = start + distance(Qin.begin() + start, max_element(Qin.begin() + start, Qin.begin() + end));
            auto Qmax = Qin[nmax];
            int ngrpor = 0;

            // GROUNDWATER DISCHARGE
            std::cout << "Groundwater" << std::endl;
            for (int n = start; n < end; ++n) {
                deltaQ[n] = Qin[n+1] - Qin[n];
                gradQ[n] = 100 * deltaQ[n] / Qin[n];

                if (n == start or n == end-1) { // ground in first and last is equal to Qin
                    Qgr[n] = Qin[n];
                    Qgrlast1 = Qin[n];
                    Qgrlast = Qin[n];
                    nlast = n;
                } else {
                    if (!par.ModeMountain && (n == nmax)) { // TODO: replace with curved interp
                        Qgr[n] = 0;
                    }

                    dQ = 100 * abs(Qin[n - 1] - Qin[n]) / Qin[n - 1];
                    dQabs = abs(Qin[n - 1] - Qin[n]);
                    dQgr = -100 * (Qgrlast - Qin[n]) / Qgrlast;
                    dQgr1 = -100 * (Qgrlast1 - Qin[n]) / Qgrlast1;
                    dQgr2 = abs(100 * (Qgrlast - Qin[n]) / ((n - nlast + 1) * Qgrlast));
                    dQgr2abs = abs((Qgrlast - Qin[n]) / (n - nlast + 1));


                    if (Qin[n] > Qgrlast or n > (nlast + 20)) {
                        auto con1 = (n - nmax > par.prodspada) and
                                    (dQ > par.grad or dQgr1 > par.kdQgr1 or dQgr2 > par.grad or
                                     dQabs > par.gradabs or dQgr2abs > par.gradabs);
                        auto con2 = dQ > par.grad1 or dQgr2 > par.grad1 or dQgr1 > par.kdQgr1 or
                                    dQabs > par.gradabs or dQgr2abs > par.gradabs;
                        if (con1 or con2)
                            continue;
                    } else {
                        auto con1 = dQ > 20 * par.grad or dQgr1 > 20 * par.kdQgr1 or dQgr2 > 20 * par.grad;
                        auto con2 = abs(Qin[n + 1] - Qin[n - 1]) < abs(Qin[n + 1] - Qin[n]);
                        if (con1 and con2)
                            continue;
                    }

                    Qgr[n] = Qin[n];
                    if (n > nmax)
                        ngrpor++;

                    if (ngrpor == 1)
                        polend[i] = n;

                    Qgrlast = Qin[n];
                    nlast = n;
                }
            }

            for (int k = start; k < end; ++k) {
                if (Qgr[k] == 0) {
                    for (int kk = k; kk < end; ++kk) {
                        if (Qgr[kk] > 0) {
                            Qgr[k] = Qgr[k - 1] + (Qgr[kk] - Qgr[k - 1]) / (kk - k + 2);
                            break;
                        }
                    }
                    dQ = 100 * abs(Qin[k - 1] - Qin[k]) / Qin[k - 1];

                    auto con1 = Qgr[k] > Qin[k];
                    auto con2 = dQ > (20 * par.grad);
                    auto con3 = abs(Qin[k + 1] - Qin[k - 1]) < abs(Qin[k + 1] - Qin[k]);

                    if (con1 and not (con2 and con3))
                        Qgr[k] = Qin[k];
                }

                Qygr[i] = Qygr[i] + Qgr[k] / ny;
                Qy[i] = Qy[i] + Qin[k] / ny;
            }

            // FLOODS AND THAWS SEPARATION

            // Check rain and thaws
            std::cout << "Thaws" << std::endl;
            for (int m = start + HalfSt; m < end - HalfSt; ++m) {

//                Psumi = 0;
//                Tsri = 0;
//                for (int u = m - HalfSt; u <= m + HalfSt; u++) {
//                    Psumi += Pin[u];
//                    Tsri += Tin[u] / p.nPav;
//                }

                Psumi = std::accumulate(Pin.begin() + m - HalfSt, Pin.begin() + m + HalfSt, 0.0);
                Tsri = std::accumulate(Tin.begin() + m - HalfSt, Tin.begin() + m + HalfSt, 0.0) / p.nPav;

                Psums[m] = Psumi;

                std::cout << "Psumi = " << Psumi << std::endl;
                std::cout << "Tsri = " << Tsri << std::endl;

                if (Psumi >= p.Pcr and Tsri >= p.Tcr1) { // critical rain
                    FactPcr[i]++;
                    FlagsPcr[m] = true;

                    std::cout << "PCR DETECTEED" << std::endl;
                }

                Tsrs[m] = Tsri;

                if (Tsri >= p.Tcr2) { // substantial plus temp
                    FactPlusTemp[i]++;
                    FlagsPlusTemp[m] = true;
                }
            }

            // Check frosts
            std::cout << "Frosts" << std::endl;
            for (int m = start + HalfStZ; m < end - HalfStZ; ++m) {

//                Tsri = 0;
//                for (int u = m - HalfStZ; u < m + HalfStZ; ++u) {
//                    Tsri += Tin[u] / p.nZam;
//                }

                Tsri = std::accumulate(Tin.begin() + m - HalfStZ, Tin.begin() + m + HalfStZ, 0.0) / p.nZam;

                if (Tsri < p.Tzam) {
                    FactMinusTemp[i]++;
                    FlagsMinusTemp[m] = true;
                }
            }

            startPol[i] = start;
            LocMax1 = start-1;
            Flex1 = start-1;
            Bend1 = nmax;
            bool minus_found = false;
            std::cout << "Rakohod " << nmax-2 << " " << startPol[i] << std::endl;

            for (auto p = nmax-2; p > startPol[i]; --p) {
                int FlexPrev = start-1;
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

                        std::cout << "start = " << start << std::endl;
                        std::cout << "Flex1 = " << Flex1 << std::endl;

                        for (auto u = Flex1; u > startPol[i]; --u) { // 602
                            if((deltaQ[u] <= -Qin[nmax] * par.SignDelta * 0.5) or ((deltaQ[u] + deltaQ[u - 1]) <= -Qin[nmax] * par.SignDelta * 0.5)) {
                                for (auto pp = u; pp < Flex1-1; ++pp) {
                                    if (deltaQ[pp] > 0) {
                                        FlexPrev = pp;
                                    }
                                }
                            }
                        } // 611


                        std::cout << "Flexprev = " << FlexPrev << std::endl;


                        if (FlexPrev >= start) {
                            LocMax1 = std::distance(Qin.begin() + FlexPrev, max_element(Qin.begin() + FlexPrev, Qin.begin() + Flex1)) + FlexPrev - 1;
                        } else {
                            LocMax1 = std::distance(Qin.begin() + start, max_element(Qin.begin() + start, Qin.begin() + Flex1)) + start;
                        } // 617

                        std::cout << "LocMax1 = " << LocMax1 << std::endl;

                        // Frosts
                        for (auto pp = LocMax1 - HalfStZ; pp < Flex1; ++pp) {
                            if (FlagsMinusTemp[pp] == true) {
                                startPol[i] = Flex1;
                                auto z = -log(Qin[Flex1] / Qin[LocMax1]) / (Flex1 - LocMax1);
                                Qo = Qin[LocMax1] / exp(-z * LocMax1);

                                for (auto qq = 1; qq < Flex1; ++qq) {
                                    Qthaw[qq] = Qin[qq] - Qgr[qq];
                                }

                                for (auto qq = Flex1; qq < nmax*2; ++qq) {
                                    if (Qo * exp(-z * qq) <= Qgr[qq]) {
                                        break;
                                    }
                                    Qthaw[qq] = Qo * exp(-z * qq) - Qgr[qq];
                                }
                                minus_found = true;
                                break;
                             }
                        }

                    }

                }

                if (minus_found)
                    break;
            }

            if (!minus_found) { // 656
                std::cout << "Search for upwards pavodks " << std::endl;

                for (auto pp = LocMax1; pp > start; --pp) {
                    if (Qin[pp] < Qin[Flex1] and (deltaQ[pp - 1] <= ((Qin[Flex1] - Qin[pp]) / (Flex1 - pp)))) {
                        Bend1 = pp;
                        break;
                    }
                }

                std::cout << "Bend1 = " << Bend1 << std::endl;

                for (auto pp = Bend1 - 2 * HalfSt; pp < LocMax1; ++pp) {
                    std::cout << "TRYIN pav " << pp << " " << FlagsPcr[pp] << std::endl;
                    if (FlagsPcr[pp] == true) { // Rain

                        std::cout << "Rain!" << Bend1 << std::endl;

                        auto afunc = (Qin[Flex1] - Qin[Bend1]) / (Flex1 - Bend1);
                        auto bfunc = Qin[Flex1] - afunc * Flex1;

                        for (auto qq = Bend1; qq < Flex1; ++qq) {
                            Qpav[qq] = Qin[qq] - (afunc * qq + bfunc);
                        }
                    }
                }
            }

            Flex2 = start-1;
            Bend2 = start-1;

            std::cout << "Hod" << std::endl;
            for (auto p = nmax; p < polend[i] - 1; ++p) {
                if ((p > Bend2) and
                  ((deltaQ[p] >= Qin[nmax] * par.SignDelta) or ((deltaQ[p] + deltaQ[p + 1]) >= Qin[nmax] * par.SignDelta))) {
                    for (auto pp = p + 1; pp > nmax; --pp) {
                        if (deltaQ[pp] < 0) {
                            Flex2 = pp + 1;
                            break;
                        }
                    }

                    for (auto pp = Flex2 + 1; pp < polend[i]; ++pp) {
                        if (((Qin[pp] < Qin[Flex2]) and (min(deltaQ[pp], deltaQ[pp - 1]) >= (Qin[pp] - Qin[Flex2]) / (pp - Flex2))) or (pp == polend[i])) {
                            Bend2 = pp;
                            for (auto ppp = Bend2 - HalfSt; ppp > Flex2 - 2*HalfSt; --ppp) {
                                if (FlagsPcr[ppp] == true) {
                                    auto z = -log(Qin[Bend2] / Qin[Flex2]) / (Bend2 - Flex2);
                                    Qo = Qin[Flex2] / exp(-z * Flex2);
                                    for (auto qq = Flex2; qq < Bend2; ++qq) {
                                        Qpav[qq] = Qin[qq] - Qo * exp(-z * qq);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}






























