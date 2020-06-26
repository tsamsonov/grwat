#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <random>
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

    static bool polfinder(const vector<int> &Year, const vector<int> &Mon, const vector<int> &Day,
                   const vector<double> &Qin, vector<int> &donep, int &iyY, const int& lstart, const parameters& par) {

        auto polgrad1 = par.polgrad1;
        auto polgrad2 = par.polgrad2;
        auto polkol1 = par.polkol1;
        auto polkol2 = par.polkol2;
        auto polkol3 = par.polkol3;

        auto polkolMount1 = par.polkolMount1;
        auto polkolMount2 = par.polkolMount2;
        auto polgradMount = par.polgradMount;

        auto p = Day.size();

        auto enter_adjustment = true;

        for (int i = 0; i < 100; ++i) { // 1398
            vector<int> Sumdonep(4, 0);
            auto vsebylo = 0;

            for (int l = 0; l < p; ++l) {
                double polQsum = 0.0;

                for (int j = 0; j < 3; ++j)
                    donep[j] = -1;

                if (Year[l] == donep[3]) {
                    if (Mon[l] >= par.polmon1 and Mon[l] <= par.polmon2 and Qin[l] != par.FlagGaps) { // 1417

                        // проверка критерия начала половодья №1 -
                        // устойчивое увеличение со средним градиентом больше или равном заданному polgrad1
                        double dQ = 0.0;
                        for (auto ff = 1; ff <= polkol1; ff++)
                            dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * polkol1);

                        if (dQ <= polgrad1)
                            donep[0] = -1;
                        else {
                            donep[0] = 1;
                            Sumdonep[0] = Sumdonep[0] + 1;
                        }

                        // проверка критерия начала половодья №2 -
                        // устойчивое увеличение не заканчивается polkol2 дней
                        dQ = 0.0;
                        for (auto ff = 1; ff <= polkol2; ff++)
                            dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * polkol2);

                        if (dQ <= 0)
                            donep[1]= -1;
                        else {
                            donep[1]= 1;
                            Sumdonep[1] = Sumdonep[1] + 1;
                        }

                        if (par.ModeMountain) {
                            // проверка критерия начала половодья №3 для гор -
                            // превышение в течении polkolMount1 каждые polkolMount2 дней расхода воды в polgradMount раз
                            donep[2] = 1;
                            for (auto ff = 1; ff <= polkolMount1; ff++) {
                                polQsum = 0.0;
                                for (auto fff = ff; fff <= ff + polkolMount2; fff++)
                                    polQsum = polQsum + Qin[l + fff];
                                if (polQsum / (Qin[l] * polkolMount2) < polgradMount)
                                    donep[2]= -1;
                            }

                            if (donep[2] == 1)
                                Sumdonep[2] = Sumdonep[2]+ 1;
                        } else {
                            // проверка критерия начала половодья №3 для равнинных рек -
                            // превышение среднего расхода воды за polkol3 дней расхода дня начала половодья (Qin[l]) в  polgrad2 раз
                            for (auto ff = 1; ff <= polkol3; ff++)
                                polQsum = polQsum + Qin[l + ff - 1];

                            if (polQsum / (Qin[l] * polkol3) < polgrad2)
                                donep[2]= -1;
                            else {
                                donep[2] = 1;
                                Sumdonep[2] = Sumdonep[2]+ 1;
                            }
                        }

                        if (donep[0] == 1 and donep[1] == 1 and donep[2] == 1) {
                            iyY = lstart + l - 1; // номер последнего члена ряда предыдущего года в общем массиве
                            donep[3] = Year[l] + 1; // следующее начало половодья уже искать в следующем году
                        }
                    }
                    // SEASONAL FLOOD IS FOUND
                    if (donep[0] == 1 and donep[1] == 1 and donep[2] == 1 and donep[3] == Year.back() + 1)
                        return true;
                }
            }

            // TODO: check whether this is really neeeded
            if (donep[0] == 1 and donep[1] == 1 and donep[2] == 1 and donep[3] == Year.back() + 1) {
                return true;
            } else if (enter_adjustment) {
                enter_adjustment = false;
            }


            // уменьшение параметров в зависимости от того, какой donep не срабатывает
            vsebylo = (Sumdonep[0] > 0 and Sumdonep[1] > 0 and Sumdonep[2] > 0) ? 1 : 0;

            auto Smallestdonep = std::min({Sumdonep[0], Sumdonep[1], Sumdonep[2]});

            std::random_device rdev;
            std::default_random_engine seed(rdev());
            std::uniform_real_distribution<> ran(0, 1);

            if (Sumdonep[0] == 0 or (vsebylo == 1 and Sumdonep[0] == Smallestdonep)) {
                auto choose = ran(seed);
                if (choose <= 0.5)
                    polgrad1 = polgrad1 - 0.1 * polgrad1;
                else if (polkol1 > 2)
                    polkol1 = polkol1 - 1;
                else
                    polgrad1 = polgrad1 - 0.1 * polgrad1;

            }

            if (Sumdonep[1] == 0 or (vsebylo == 1 and Sumdonep[1] == Smallestdonep)) {
                polkol2 = polkol2 - 1;
            }

            if (Sumdonep[2] == 0 or (vsebylo == 1 and Sumdonep[2] == Smallestdonep)) {
                auto choose = ran(seed);
                if (par.ModeMountain) {
                    if (choose <= 0.2) {
                        polkolMount1 = polkolMount1 - 5;
                    } else if (choose > 0.2 and choose <= 0.6) {
                        if (polkolMount2 > 1)
                            polkolMount1 = polkolMount1 - 1;
                        else
                            polgradMount = polgradMount - 0.1 * polgradMount;
                    } else {
                        polgradMount = polgradMount - 0.1 * polgradMount;
                    }
                } else {
                    if (choose <= 0.5) {
                        polgrad2 = polgrad2 - 0.1 * polgrad2;
                    } else if (polkol3 > par.polkol3 / 2) {
                        polkol3 = polkol3 - 5;
                    } else {
                        polgrad2 = polgrad2 - 0.1 * polgrad2;
                    }
                }
            }

        }

        return false; // SEASONAL FLOOD NOT FOUND
    }

    static void separate(const vector<int>& Year, const vector<int>& Mon, const vector<int>& Day,
                  const vector<double>& Qin, const vector<double>& Tin, const vector<double>& Pin,
                  vector<double>& Qgr, vector<double>& Qpol, vector<double>& Qpav,
                  vector<double>& Qthaw, vector<double>& Qpb, const parameters& par) {

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

        // Initialize vectors

        set<int> years(Year.begin(), Year.end());
        auto nyears = years.size();

        vector<int> iy(nyears, -99); // indices of water resource years ends
        vector<int> ny(nyears, 0);
        vector<int> YGaps(nyears, 0);
        vector<int> donep(4, -1); // four criteria of seasonal discharge beginning
        map<int, int> NumGapsY;
        auto NumGaps = 0;
        auto y = Year[0];
        auto ng = 0; // number of the first year
        double polQsum = 0.0;

        // auto iy1 = 0;
        // auto iylast = -1;

        auto iylast = 0;
        donep[3] = y;

        auto w = Qin.size();
        for (auto l = 0; l < w; l++) { // 177

            donep[0] = donep[1] = donep[2] = -1;

            if (Qin[l] == par.FlagGaps) { // 185
                if (Year[l] != y) {
                    NumGapsY[y] = NumGaps;
                    y = Year[l];
                }
                NumGaps++;
            }

            if (Year[l] > donep[3]) { // 190
                if (NumGapsY[Year[l] - 1] >= 30) {

                    iy[ng] = -99;
                    // if (ng > 0)
                    //   iy[ng - 1] = -99;

                    YGaps[ng] = 1;
                    YGaps[ng - 1] = 1;
                    ng++;
                    NumGaps = 0;
                    donep[3] = Year[l];
                } else {
                    auto begin = (iylast >= 0) ? iy[iylast] + 300 : 0;
                    // auto begin = (iylast >=0) ? iy[iylast] + 300 : (iy1 > 0) ? iy1 + 300: 0
                    auto lengthcrop = l - begin + 1;
                    auto end = begin + lengthcrop - 1;

                    // int& iyY = ng > 0 ? iy[ng-1] : iy1;
                    int &iyY = iy[ng];

                    vector<double> Qcrop(Qin.begin() + begin, Qin.begin() + end);
                    vector<int> Yearcrop(Year.begin() + begin, Year.begin() + end);
                    vector<int> Moncrop(Mon.begin() + begin, Mon.begin() + end);
                    vector<int> Daycrop(Day.begin() + begin, Day.begin() + end);

                    // POLFINDER
                    std::cout << "Polfinder: " << donep[3] << std::endl;
                    auto polfound = polfinder(Yearcrop, Moncrop, Daycrop, Qcrop, donep, iyY, begin, par);
                    if (polfound) {
                        // iylast = ng - 1;
                        iylast = ng;
                    } else {
                        cout << "Failed" << endl;
                        donep[3]++;
                    }
                    ng++;
                }
            }

            if (Year[l] == donep[3]) { //221
                if (Mon[l] >= par.polmon1 and Mon[l] <= par.polmon2 and Qin[l] != par.FlagGaps) { //223
                    double dQ = 0.0;
                    auto proceed = true;
                    for (auto ff = 1; ff <= par.polkol1; ff++) {
                        if (Qin[l + ff] == par.FlagGaps or Qin[l + ff - 1] == par.FlagGaps) { // goto 8787
                            proceed = false;
                            break;
                        } else {
                            dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * par.polkol1);
                        }
                    }

                    if (proceed) {
                        if (dQ <= par.polgrad1) {
                            donep[0] = -1;
                        } else {
                            donep[0] = 1;
                        }

                        dQ = 0.0;
                        for (auto ff = 1; ff <= par.polkol2; ff++) {
                            dQ = dQ + 100 * (Qin[l + ff] - Qin[l + ff - 1]) / (Qin[l + ff - 1] * par.polkol2);
                        }

                        if (dQ <= 0) {
                            donep[1] = -1;
                        } else {
                            donep[1] = 1;
                        }

                        if (par.ModeMountain) {
                            donep[2] = 1;
                            for (auto ff = 1; ff <= par.polkolMount1; ff++) {
                                polQsum = 0.0;
                                for (auto fff = ff; fff <= par.polkolMount2; fff++)
                                    polQsum = polQsum + Qin[l + fff];
                                if (polQsum / (Qin[l] * par.polkolMount2) < par.polgradMount)
                                    donep[2] = -1;
                            }
                        } else {
                            polQsum = 0.0;
                            for (auto ff = 1; ff <= par.polkol3; ff++)
                                polQsum = polQsum + Qin[l + ff - 1];

                            if (polQsum / (Qin[l] * par.polkol3) < par.polgrad2)
                                donep[2] = -1;
                            else
                                donep[2] = 1;
                        }
                    }

                    if (donep[0] == 1 and donep[1] == 1 and donep[2] == 1) {
//                        if (ng > 0) {
//                            iy[ng - 1] = l - 1;
//                            iylast = ng - 1;
//                        } else {
//                            iy1 = l - 1;
//                        }
                        iy[ng] = l - 1;
                        iylast = ng;

                        donep[3] = Year[l] + 1; // search next seasonal flood in the next year
//                        if (l > 1 and NumGaps > 0) {
//                            YGaps[ng - 1] = 1;
//                            NumGaps = 0;
//                        }

                        if (NumGaps > 0) {
                            YGaps[ng] = 1;
                            NumGaps = 0;
                        }
                        ng++; // number of years
                    }
                }
            }
        }

        cout << "Water resources year beginnings:" << endl;

//        Qpol[iy1 + 1] = 1;
//
//        cout << Day[iy1 + 1] << '-' << Mon[iy1 + 1] << '-' << Year[iy1 + 1] << endl;

        for (auto i: iy) {
            Qpol[i + 1] = 1;
            cout << Day[i + 1] << '-' << Mon[i + 1] << '-' << Year[i + 1] << endl;
        }
    }
}



