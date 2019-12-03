#include <iostream>
#include <ctime>
#include <cmath>
using namespace std;

enum matrixType{
    valueType,
    idType,
    randomType
};

template <class T>
class Matrix{
    private:
        T** ptr;
        int matrixRow;
        int matrixColumn;
        matrixType type;
    public:
        Matrix();
        Matrix(int , int , T);
        Matrix(int , int , char);
        T reach(int , int);
        void resize(int , int);
        void print() const;
        void print(string) const;
        Matrix operator+ (Matrix);
        Matrix operator+ (int);
        Matrix operator- (Matrix);
        Matrix operator- (int);
        Matrix operator* (Matrix);
        Matrix operator* (int);
        Matrix operator/ (int);
        Matrix operator% (int);
        Matrix operator^ (int);
};

template <class T>
T Matrix<T>::reach(int row , int column){
    return (this->ptr)[row][column];
}


template <class T>
Matrix<T>::Matrix(){
    this->ptr = new int*[10];
    for(int i = 0 ; i < 10; i++){
        this->ptr[i] = new int[10];
    }
    this->matrixRow = this->matrixColumn = 10;
    this->type = valueType;
    for(int i = 0 ; i < 10 ; i++){
        for(int j = 0 ; j < 10 ; j++){
            reach(i , j) = 0;
        }
    }
}

template <class T>
Matrix<T>::Matrix(int row , int column , T value){
    this->ptr = new T*[row];
    for(int i = 0; i < row ; i++){
        this->ptr[i] = new T[column];
    }
    this->matrixRow = row;
    this->matrixColumn = column;
    this->type = valueType;
    for(int i = 0 ; i < row ; i++){
        for(int j = 0 ; j < column ; j++){
            reach(i , j) = value;
        }
    }
}

template <class T>
Matrix<T>::Matrix(int row , int column , char value){
    this->ptr = new T*[row];
    for(int i = 0; i < row ; i++){
        this->ptr[i] = new T[column];
    }
    this->matrixRow = row;
    this->matrixColumn = column;
    if(value == 'e'){                   //identification matrix
        for(int i = 0 ; i < row ; i++){
            for(int j = 0 ; j < column ; j++){
                if(i == j) reach(i , j) = 1;
                else reach(i , j) = 0;
            }
        }
        this->type = idType;
    }
    if(value == 'r'){                  //random integer matrix
        srand(time(NULL));
        for(int i = 0 ; i < row ; i++){
            for(int j = 0 ; j < column ; j++){
                reach(i , j) = rand() % 256;
            }
        }
        this->type = randomType;
    }
}

template <class T>
void Matrix<T>::resize(int row , int column){
    T** tempArray = new T*[row];
    for(int i = 0; i < row ; i++){
        tempArray[i] = new T[column];
    }
    if(this->type == valueType){      //if integer matrix
        T value = reach(1 , 1);
        for(int i = 0 ; i < row ; i++){
            for(int j = 0 ; j < column ; j++){
                tempArray[i][j] = value;
            }
        }
    }
    if(this->type == idType){         //if identification matrix
        for(int i = 0 ; i < row ; i++){
            for(int j = 0 ; j < column ; j++){
                if(i == j) tempArray[i][j] = 1;
                else tempArray[i][j] = 0;
            }
        }
    }
    if(this->type == randomType){     //if random integer matrix
        for(int i = 0 ; i < row ; i++){
            for(int j = 0 ; j < column ; j++){
                tempArray[i][j] = rand() % 256;
            }
        }
    }
    //delete operations
    for(int i = 0; i < this->matrixRow ; i++){
        delete[] this->ptr[i];
    }
    delete[] this->ptr;

    //assigning new matrix
    this->ptr = tempArray;
    this->matrixColumn = column;
    this->matrixRow = row;

}

template <class T>
void Matrix<T>::print()const{
    for(int i = 0 ; i < this->matrixRow; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            cout << reach(i , j) << " ";
        }
        cout << endl;
    }
}

template <class T>
void Matrix<T>::print(string filename)const{
    fstream fname;
    fname.open(filename);
    for(int i = 0 ; i < this->matrixRow; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            fname << reach(i , j) << " ";
        }
        fname << endl;
    }
    fname.close();
}

template <class T>
Matrix<T> Matrix<T>::operator+(Matrix operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) 
                                + operand.reach(i , j);
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator-(Matrix operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) 
                                - operand.reach(i , j);
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator*(Matrix operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , operand.matrixColumn);
    int value = 0;
    for(int i = 0; i < this->matrixRow ; i++){
        for(int j = 0 ; j < operand.matrixColumn ; j++){    //offset
            for(int k = 0 ; k < this->matrixColumn ; k++){
                value += this->reach(i , k) * operand.reach(k , j);
            }
            temp.reach(i , j) = value;
            value = 0;
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator+(int operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) + operand;
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator-(int operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) - operand;
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator*(int operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) * operand;
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator/(int operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) / operand;
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator%(int operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) % operand;
        }
    }
    return temp;
}

template <class T>
Matrix<T> Matrix<T>::operator^(int operand){
    Matrix<T> temp;
    temp.resize(this->matrixRow , this->matrixColumn);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = pow(this->reach(i , j) , operand);
        }
    }
    return temp;
}