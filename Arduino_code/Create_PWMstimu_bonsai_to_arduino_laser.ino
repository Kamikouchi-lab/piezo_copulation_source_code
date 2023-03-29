//library
#include <avr/io.h>

#define PWMPin 10 //LED connected to PWMpin 10
const int Inputpin = 4 ; //PC connected to digital pin 4


//defenition
//frq:frequency (1Hz~)
//duty:duty ratio
void Hzwrite(int frq, float duty) { 

    // mode
  TCCR1A = 0b00100001;
  TCCR1B = 0b00010100; 

  // TOP値 value
  OCR1A = (unsigned int)(31250 / frq);

  // Duty ratio
  OCR1B = (unsigned int)(31250 / frq * duty);
}

void setup() {
  pinMode(PWMPin, OUTPUT); 
  pinMode(Inputpin, INPUT);
  digitalWrite(PWMPin, LOW); //defalt state is LOW
  Serial.begin( 9600 );
  
}

void loop() {

  int value;
  value = 0;
  value = digitalRead( Inputpin );
  Serial.print(value);

  if (value >= 1){
    delay(2000);
    value = digitalRead( Inputpin );
    if (value >= 1){
      //Serial.print(value);
      delay(3000);
      value = digitalRead( Inputpin );
      if (value >= 1){
        //Serial.print(value);
        delay(10000);
        value = digitalRead( Inputpin );
        while (value >= 1){
            Hzwrite(30, 0.5);
            delay(30000);
            digitalWrite(PWMPin, LOW);
            delay(30000);
            value = digitalRead( Inputpin );
        }
      }
      else{
      digitalWrite(PWMPin, LOW);
      }
    }
    else{
      digitalWrite(PWMPin, LOW);
    }
  }
  else{
    digitalWrite(PWMPin, LOW);
  }
}
 
