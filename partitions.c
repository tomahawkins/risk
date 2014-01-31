#include <stdio.h>

#include "risk_lib.h"

void sensor_1_main      (void) { for (;;) { printf("Hi, I'm sensor_1.\n"     ); risk_yield(); } }
void sensor_2_main      (void) { for (;;) { printf("Hi, I'm sensor_2.\n"     ); risk_yield(); } }
void sensor_3_main      (void) { for (;;) { printf("Hi, I'm sensor_3.\n"     ); risk_yield(); } }
void sensor_voting_main (void) { for (;;) { printf("Hi, I'm sensor_voting.\n"); risk_yield(); } }
void control_law_1_main (void) { for (;;) { printf("Hi, I'm control_law_1.\n"); risk_yield(); } }
void control_law_2_main (void) { for (;;) { printf("Hi, I'm control_law_2.\n"); risk_yield(); } }
void actuator_1_main    (void) { for (;;) { printf("Hi, I'm actuator_1.\n"   ); risk_yield(); } }
void actuator_2_main    (void) { for (;;) { printf("Hi, I'm actuator_2.\n"   ); risk_yield(); } }
void actuator_3_main    (void) { for (;;) { printf("Hi, I'm actuator_3.\n"   ); risk_yield(); } }

