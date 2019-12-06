#include <iostream>
#include <fstream>
#include <ctime>
#include <cmath>
using namespace std;

template <class C>
double determinant(C** arr , int width){
    if(width == 2){
        return  (arr[0][0] * arr[1][1]) -
                (arr[1][0] * arr[0][1]);
    }else{
        double detValue = 0;
        C newArray[width - 1][width - 1];
        for(int i = 0 ; i < width ; i++){
            //parent matrixin ilk satirinde gezinen for dongusu
            for(int childRow = 0 ; childRow < (width - 1) ; childRow++){
                //child matrixde satiri gezinen for dongusu
                int offset = 0;
                for(int childCol = 0 ; childCol < width ; childCol++){
                    if(childCol == i){
                        offset = 1;
                        continue;
                    }
                    newArray[childRow][childCol - offset] = 
                            arr[childRow + 1][childCol];
                }
                offset = 0;
            }
            detValue += arr[0][i] * pow(-1 , i) * determinant(newArray
                                                           , width - 1);
        }
        return detValue;
    }

}

template <class C>
C** cof(C** arr , int rowIndex , int colIndex , int arrWidth){
    C newArray[arrWidth - 1][arrWidth - 1];
    int rowOffset = 0;
    int columnOffset = 0;
    for(int childRow = 0 ; childRow < arrWidth ; childRow++){
        if(childRow == rowIndex){
            rowOffset = 1;
            continue;
        }
        for(int childCol = 0 ; childCol < arrWidth ; childCol++){
            if(childCol == colIndex){
                columnOffset = 1;
                continue;
            }
            newArray[childRow - rowOffset][childCol - columnOffset] = 
            arr[childRow][childCol];
        }
        columnOffset = 0;
    }
    return newArray;
}

enum matrixType{
    valueType,
    idType,
    randomType
};

template <class C>
class Matrix{
    private:
        C** ptr;
        int matrixRow;
        int matrixColumn;
        matrixType type;
    public:
        Matrix();
        Matrix(int , int , C);
        Matrix(int , int , char);
        C reach(int , int);
        void resize(int , int);
        void print() const;
        void print(string);
        Matrix operator+ (Matrix&);
        Matrix operator+ (int);
        Matrix operator- (Matrix&);
        Matrix operator- (int);
        Matrix operator* (Matrix&);
        Matrix operator* (int);
        Matrix operator/ (int);
        Matrix operator% (int);
        Matrix operator^ (int);
        Matrix T();
        Matrix emul(Matrix&);
        Matrix inv();
        double det();
};

template <class C>
C Matrix<C>::reach(int row , int column){
    return (this->ptr)[row][column];
}


template <class C>
Matrix<C>::Matrix(){
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

template <class C>
Matrix<C>::Matrix(int row , int column , C value){
    this->ptr = new C*[row];
    for(int i = 0; i < row ; i++){
        this->ptr[i] = new C[column];
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

template <class C>
Matrix<C>::Matrix(int row , int column , char value){
    this->ptr = new C*[row];
    for(int i = 0; i < row ; i++){
        this->ptr[i] = new C[column];
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

template <class C>
void Matrix<C>::resize(int row , int column){
    C** tempArray = new C*[row];
    for(int i = 0; i < row ; i++){
        tempArray[i] = new C[column];
    }
    if(this->type == valueType){      //if integer matrix
        C value = reach(1 , 1);
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

template <class C>
void Matrix<C>::print()const{
    for(int i = 0 ; i < this->matrixRow; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            cout << reach(i , j) << " ";
        }
        cout << endl;
    }
}

template <class C>
void Matrix<C>::print(string filename){
    ofstream outputFile;
    outputFile.open(filename);
    for(int i = 0 ; i < this->matrixRow; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            outputFile << reach(i , j) << " ";
        }
        outputFile << endl;
    }
    outputFile.close();
}

template <class C>
Matrix<C> Matrix<C>::operator+(Matrix& operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) 
                                + operand->reach(i , j);
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator-(Matrix& operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) 
                                - operand->reach(i , j);
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator*(Matrix& operand){
    Matrix<C> temp(this->matrixRow , operand.matrixColumn , 0);
    int value = 0;
    for(int i = 0; i < this->matrixRow ; i++){
        for(int j = 0 ; j < operand.matrixColumn ; j++){    //offset
            for(int k = 0 ; k < this->matrixColumn ; k++){
                value += this->reach(i , k) * operand->reach(k , j);
            }
            temp.reach(i , j) = value;
            value = 0;
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator+(int operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) + operand;
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator-(int operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) - operand;
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator*(int operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) * operand;
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator/(int operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) / operand;
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator%(int operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) % operand;
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::operator^(int operand){
    Matrix<C> temp(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0 ; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            temp.reach(i , j) = pow(this->reach(i , j) , operand);
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::T(){
    Matrix<C> temp(this->matrixColumn , this->matrixRow , 0);
    for(int i = 0; i < this->matrixRow ; i++){
        for(int j = 0; j < this->matrixColumn ; j++){
            temp.reach(j , i) = this->reach(i , j);
        }
    }
    return temp;
}

template <class C>
Matrix<C> Matrix<C>::emul(Matrix& operand){
    Matrix<C> temp(this->matrixColumn , this->matrixRow , 0);
    for(int i = 0; i < this->matrixRow ; i++){
        for(int j = 0; j < this->matrixColumn ; j++){
            temp.reach(i , j) = this->reach(i , j) 
                                * operand->reach(i , j);
        }
    }
    return temp;
}

template <class C>
double Matrix<C>::det(){
    if(this->matrixRow == this->matrixColumn){
        if(this->matrixRow == 2){
            return  (this->reach(0 , 0) * this->reach(1 , 1)) -
                    (this->reach(1 , 0) * this->reach(0 , 1));
        }else{
            double detValue = 0;
            C newArray[this->matrixRow - 1][this->matrixColumn - 1];
            for(int i = 0 ; i < this->matrixColumn ; i++){
                //parent matrixin ilk satirinde gezinen for dongusu
                for(int childRow = 0; childRow < (this->matrixRow - 1);
                                                            childRow++){
                    //child matrixde satiri gezinen for dongusu
                    int offset = 0;
                    for(int childCol = 0; childCol < this->matrixColumn;
                                                            childCol++){
                        if(childCol == i){
                            offset = 1;
                            continue;
                        }
                        newArray[childRow][childCol - offset] = 
                                this->reach(childRow + 1 , childCol);
                    }
                    offset = 0;
                }
                detValue += reach(0 , i) * pow(-1 , i) * 
                         determinant(newArray , this->matrixColumn - 1);
            }
            return detValue;
        }
    }else{
        return 0;
    }
}

template <class C>
Matrix<C> Matrix<C>::inv(){
    Matrix<C> newArray(this->matrixRow , this->matrixColumn , 0);
    for(int i = 0; i < this->matrixRow ; i++){
        for(int j = 0 ; j < this->matrixColumn ; j++){
            newArray.reach(i , j) = determinant(cof(this->ptr , i , j , 
                               this->matrixColumn), this->matrixColumn);
        }
    }
    return newArray * (1 / this->det());
}

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

struct rgb
{
    unsigned int red;
    unsigned int green;
    unsigned int blue;
    rgb();
    rgb(unsigned int , unsigned int , unsigned int);
};

rgb::rgb(){
    this->red = 0;
    this->green = 0;
    this->blue = 0;
}
rgb::rgb(unsigned int r , unsigned int g , unsigned int b){
    this->red = r;
    this->green = g;
    this->blue = b;
}

template <class C>
class Image : public Matrix<C> {
    private:
        rgb rgb;
    public:
        Image();
        Image(int , int);
};

template <class C>
Image<C>::Image(){
    Matrix<C>(255 , 255 , rgb);
}

template <class C>
Image<C>::Image(int width , int height){
    Matrix<C>(width , height , rgb);
}
