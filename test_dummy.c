
void main() {
    int a;
    int b;
    int c;
    int d;
    int e;
    a = 1; //This stmt is 15 lines of code
    b = 2; //Same;
    c = a;
    cout << "Enter value for e ";
    cin >> e;
    d = a + b;
    //e = b + c;
    c = a + e; 
    cout << c;
    cout << "\n";
    cout << d; //Printing wrong, acting right
    cout << "\n";
    cout << e;
    cout << "\n";

    a = d*b; 
    d = e/d; //d being set not static here
    cout << a; 
    cout << "\n";
    cout << d;
}
