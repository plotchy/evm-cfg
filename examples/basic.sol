pragma solidity ^0.8.19;
contract ez{
    uint a;
    uint b;
    fallback() external payable{
        if (a == 0){
            a = 1;
        }
        else if (a == 1){
           for (uint i = 0; i < 10; i++){
               b += i;
           }
        } else {
            a = 0;
        }
    }
}