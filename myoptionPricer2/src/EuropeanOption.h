#include<vector>

class EuropeanOption{
public:

  //constructor
  EuropeanOption(
    int nInt_,
    double strike_,
    double spot_,
    double vol_,
    double r_,
    double expiry_,
    double barrier_
  );

  //destructor
  ~EuropeanOption(){};

  //methods
  void generatePath();
  double getArithmeticMean();
  double getGeometricMean();
  void printPath();
  double getArithmeticEuropeanCallPrice(int nReps);
  double getEuropeanUpAndInCallPrice(int nReps);
  double getArithmeticEuropeanPutPrice(int nReps);
  double getGeometricEuropeanCallPrice(int nReps);
  double getGeometricEuropeanPutPrice(int nReps);
  double operator()(char char1, char char2, int nReps);

  //members
  std::vector<double> thisPath;
  int nInt;
  double strike;
  double spot;
  double vol;
  double r;
  double expiry;
  double barrier;

};
