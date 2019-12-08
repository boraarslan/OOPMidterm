#include <iostream>
#include <fstream>
#include <ctime>
#include <cmath>
#include <string>
using namespace std;

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
    protected:
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



unsigned int normalise(char value){
    return (unsigned int)((unsigned char)value);
}

enum imageType{
    bmp ,
    bin ,
    zero
};

template <class C>
class Image : public Matrix<C> {
    private:
        bool** binaryImage;
        unsigned int imageWidth;
        unsigned int imageHeight;
        char bmpHeader[54];
        char* imageTable;
        unsigned int pixelValueOffset;
        imageType imageInfo;
    public:
        Image();
        Image(int , int);
        Image(string , string);
        void setRed(int , int , unsigned int);
        void setGreen(int , int , unsigned int);
        void setBlue(int , int , unsigned int);
        void setRGB(int , int ,
                    unsigned int , unsigned int , unsigned int);
        void imread(string , string);
        void imwrite(string , string);
        void color2gray();
        void gray2binary(int);
        void erosion();
        void dilation();
        void opening();
        void closing();
};

template <class C>
void Image<C>::setRed(int indexRow , 
                      int indexColumn ,
                      unsigned int Value){

    this->ptr[indexRow][indexColumn].red = Value;
}

template <class C>
void Image<C>::setGreen(int indexRow , 
                      int indexColumn ,
                      unsigned int Value){

    this->ptr[indexRow][indexColumn].green = Value;
}

template <class C>
void Image<C>::setBlue(int indexRow , 
                      int indexColumn ,
                      unsigned int Value){

    this->ptr[indexRow][indexColumn].blue = Value;
}

template <class C>
void Image<C>::setRGB(int indexRow ,
                      int indexColumn ,
                      unsigned int redValue , 
                      unsigned int greenValue , 
                      unsigned int blueValue){
                          
    setRed(indexRow , indexColumn , redValue);
    setGreen(indexRow , indexColumn , greenValue);
    setBlue(indexRow , indexColumn , blueValue);

}


template <class C>
Image<C>::Image(): Matrix<C>(255 , 255 , rgb()){
    
}

template <class C>
Image<C>::Image(int width , int height): 
                Matrix<C>(width , height , rgb()){
    
}

template <class C>
Image<C>::Image(string filename , string format): 
                Matrix<C>(10 , 10 , rgb()){
    if(format.compare("bmp") == 0){
        ifstream bmpFile(filename , ios::in | ios::binary);
        char byteValue;

        bmpFile.read(this->bmpHeader , 54);
        this->pixelValueOffset = this->bmpHeader[10] |
                                 this->bmpHeader[11] << 8 |
                                 this->bmpHeader[12] << 16 |
                                 this->bmpHeader[13] << 24;
        this->imageWidth =       this->bmpHeader[18] |
                                 this->bmpHeader[19] << 8 |
                                 this->bmpHeader[20] << 16 |
                                 this->bmpHeader[21] << 24;
        this->imageHeight =      this->bmpHeader[22] |
                                 this->bmpHeader[23] << 8 |
                                 this->bmpHeader[24] << 16 |
                                 this->bmpHeader[25] << 24;
        char localTable[(this->pixelValueOffset) - 54];
        bmpFile.read(localTable , (this->pixelValueOffset) - 54);
        this->imageTable = localTable;

        Matrix<C>::resize(this->imageHeight , this->imageWidth);

        bmpFile.seekg(this->pixelValueOffset);

        for(int i = 1; i <= this->imageHeight ; i++){
            for(int j = 0; j < this->imageWidth ; j++){
                bmpFile.read(&byteValue , 1);
                setRGB(this->imageHeight - i , j ,
                       0 , normalise(byteValue) , 0);
            }
        }
        bmpFile.close();
        this->imageInfo = bmp;
    }
    if(format.compare("bin") == 0){
        char byteValue;
        ifstream binFile(filename , ios::in | ios::binary);

        binFile.read(&byteValue , 1);
        this->imageHeight = normalise(byteValue);
        binFile.read(&byteValue , 1);
        this->imageWidth = normalise(byteValue);

        Matrix<C>::resize(this->imageHeight , this->imageWidth);

        for(int i = 0; i < this->imageHeight ; i++){
            for(int j = 0; j < this->imageWidth ; j++){
                binFile.read(&byteValue , 1);
                setRGB(i , j , 0 , normalise(byteValue) , 0);
            }
        }
        binFile.close();
        this->imageInfo = bin;
    }
}

template <class C>
void Image<C>::imread(string filename , string format){

    if(format.compare("bmp") == 0){
        ifstream bmpFile(filename , ios::in | ios::binary);
        char byteValue;

        bmpFile.read(this->bmpHeader , 54);
        this->pixelValueOffset = this->bmpHeader[10] |
                                 this->bmpHeader[11] << 8 |
                                 this->bmpHeader[12] << 16 |
                                 this->bmpHeader[13] << 24;
        this->imageWidth =       this->bmpHeader[18] |
                                 this->bmpHeader[19] << 8 |
                                 this->bmpHeader[20] << 16 |
                                 this->bmpHeader[21] << 24;
        this->imageHeight =      this->bmpHeader[22] |
                                 this->bmpHeader[23] << 8 |
                                 this->bmpHeader[24] << 16 |
                                 this->bmpHeader[25] << 24;
        char localTable[(this->pixelValueOffset) - 54];
        bmpFile.read(localTable , (this->pixelValueOffset) - 54);
        this->imageTable = localTable;

        Matrix<C>::resize(this->imageHeight , this->imageWidth);

        bmpFile.seekg(this->pixelValueOffset);

        for(int i = 1; i <= this->imageHeight ; i++){
            for(int j = 0; j < this->imageWidth ; j++){
                bmpFile.read(&byteValue , 1);
                setRGB(this->imageHeight - i , j ,
                       0 , normalise(byteValue) , 0);
            }
        }
        bmpFile.close();
        this->imageInfo = bmp;
    }
    if(format.compare("bin") == 0){
        char byteValue;
        ifstream binFile(filename , ios::in | ios::binary);

        binFile.read(&byteValue , 1);
        this->imageHeight = normalise(byteValue);
        binFile.read(&byteValue , 1);
        this->imageWidth = normalise(byteValue);
        Matrix<C>::resize(this->imageHeight , this->imageWidth);

        for(int i = 0; i < this->imageHeight ; i++){
            for(int j = 0; j < this->imageWidth ; j++){
                binFile.read(&byteValue , 1);
                setRGB(i , j , 0 , normalise(byteValue) , 0);
            }
        }
        binFile.close();
        this->imageInfo = bin;
    }
}

template <class C>
void Image<C>::imwrite(string filename , string format){
    if(format.compare("bmp") == 0){
        ofstream bmpFile(filename , ios::out | ios::binary);
        char byteValue;
        bmpFile.write(this->bmpHeader , 54);
        bmpFile.write(this->imageTable , (this->pixelValueOffset) - 54);
        for(int i = 1; i <= this->imageHeight ; i++){
            for(int j = 0; j < this->imageWidth ; j++){
                byteValue = 
                      Matrix<C>::reach(this->imageHeight - i , j).green;
                bmpFile.write(&byteValue , 1);
            }
        }
        bmpFile.close();
    }
    if(format.compare("bin") == 0){
        char byteValue;
        ofstream binFile(filename , ios::out | ios::binary);

        byteValue = this->imageHeight;
        binFile.write(&byteValue , 1);
        byteValue = this->imageWidth;
        binFile.write(&byteValue , 1);

        for(int i = 0; i < this->imageHeight ; i++){
            for(int j = 0; j < this->imageWidth ; j++){
                byteValue = Matrix<C>::reach(i , j).green;
                binFile.write(&byteValue , 1);
            }
        }
        binFile.close();
    }
}

template <class C>
void Image<C>::color2gray(){
    for(int i = 0; i < this->imageHeight ; i++){
        for(int j = 0 ; j < this->imageWidth ; j++){
            setRed(i , j , 0);
            setBlue(i , j , 0);
        }
    }
}

template <class C>
void Image<C>::gray2binary(int thr){
    bool **temp = new bool*[this->imageHeight];
    for(int i = 0; i < this->imageHeight; i++){
        temp[i] = new bool[this->imageWidth];
    }
    for(int i = 0; i < this->imageHeight ; i++){
        for(int j = 0; j < this->imageWidth ; j++){
            if(Matrix<C>::reach(i , j).green < thr){
                temp[i][j] = 0;
            } else {
                temp[i][j] = 1;
            }
        }
    }
    this->binaryImage = temp;
    this->imageInfo = zero;
}

template <class C>
void Image<C>::erosion(){
    if(this->imageInfo == zero){
        bool** temp = new bool*[this->imageHeight + 2];
        for(int i = 0; i < (this->imageHeight + 2) ; i++){
            temp[i] = new bool[this->imageWidth + 2];
        }

        for(int i = 0; i < (this->imageWidth + 2); i++){
            temp[0][i] = 1;
            temp[this->imageHeight + 1][i] = 1;
        }
        for(int i = 0; i < (this->imageHeight + 2); i++){
            temp[i][0] = 1;
            temp[i][this->imageWidth + 1] = 1;
        }

        for(int i = 0; i < this->imageHeight ; i++){
            for(int j = 0 ; j < this->imageWidth ; j++){
                temp[i + 1][j + 1] = this->binaryImage[i][j];
            }
        }

        bool** finalImage = new bool*[this->imageHeight];
        for(int i = 0; i < (this->imageHeight) ; i++){
            finalImage[i] = new bool[this->imageWidth];
        }

        for(int i = 1; i <= this->imageHeight ; i++){
            for(int j = 1; j <= this->imageWidth ; j++){
                if(temp[i][j] == 0         ||
                   temp[i - 1][j - 1] == 0 ||
                   temp[i - 1][j]     == 0 ||
                   temp[i - 1][j + 1] == 0 ||
                   temp[i][j - 1]     == 0 ||
                   temp[i][j + 1]     == 0 ||
                   temp[i + 1][j - 1] == 0 ||
                   temp[i + 1][j]     == 0 ||
                   temp[i + 1][j + 1] == 0){

                       finalImage[i - 1][j - 1] = 0;

                   }else{
                       finalImage[i - 1][j - 1] = 1;
                   }
            }
        }

        delete this->binaryImage;
        delete temp;
        this->binaryImage = finalImage;

    }else{
        return;
    }
}

template <class C>
void Image<C>::dilation(){
    if(this->imageInfo == zero){
        bool** temp = new bool*[this->imageHeight + 2];
        for(int i = 0; i < (this->imageHeight + 2) ; i++){
            temp[i] = new bool[this->imageWidth + 2];
        }

        for(int i = 0; i < (this->imageWidth + 2); i++){
            temp[0][i] = 0;
            temp[this->imageHeight + 1][i] = 0;
        }
        for(int i = 0; i < (this->imageHeight + 2); i++){
            temp[i][0] = 0;
            temp[i][this->imageWidth + 1] = 0;
        }

        for(int i = 0; i < this->imageHeight ; i++){
            for(int j = 0 ; j < this->imageWidth ; j++){
                temp[i + 1][j + 1] = this->binaryImage[i][j];
            }
        }

        bool** finalImage = new bool*[this->imageHeight];
        for(int i = 0; i < (this->imageHeight) ; i++){
            finalImage[i] = new bool[this->imageWidth];
        }

        for(int i = 1; i <= this->imageHeight ; i++){
            for(int j = 1; j <= this->imageWidth ; j++){
                if(temp[i][j] == 1         ||
                   temp[i - 1][j - 1] == 1 ||
                   temp[i - 1][j]     == 1 ||
                   temp[i - 1][j + 1] == 1 ||
                   temp[i][j - 1]     == 1 ||
                   temp[i][j + 1]     == 1 ||
                   temp[i + 1][j - 1] == 1 ||
                   temp[i + 1][j]     == 1 ||
                   temp[i + 1][j + 1] == 1){

                       finalImage[i - 1][j - 1] = 1;

                   }else{
                       finalImage[i - 1][j - 1] = 0;
                   }
            }
        }

        delete this->binaryImage;
        delete temp;
        this->binaryImage = finalImage;

    }else{
        return;
    }
}

template <class C>
void Image<C>::opening(){
    erosion();
    dilation();
}

template <class C>
void Image<C>::closing(){
    dilation();
    erosion();
}
