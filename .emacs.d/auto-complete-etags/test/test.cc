class TestClass {
public:
  void normal_func() {}
  int get() const { return 0; }
  void set(int i) { i_ = i; }

  // overloaded functions
  void overloaded_func(int i);
  void overloaded_func(double d);

private:
  int i_;
};

