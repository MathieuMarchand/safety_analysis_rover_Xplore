#include <memory>
#include "rclcpp/rclcpp.hpp"
#include "tutorial_interfaces/msg/num.hpp" 
#include "std_msgs/msg/int8.hpp" 
#include "std_msgs/msg/int64.hpp"   
#include "motor_control_interfaces/msg/motor_command.hpp"
#include "motor_control_interfaces/msg/motor_data.hpp"
#include "avionics_interfaces/msg/spectro_response.hpp"
#include "avionics_interfaces/msg/spectro_request.hpp"

using std::placeholders::_1;
using namespace std::chrono_literals;



// =================================================================================
/**
 * @brief 
 * 
 */
class SC_FSM_Drill : public rclcpp::Node {

  public:

    rclcpp::Subscription<std_msgs::msg::Int8>::SharedPtr fsm_sc;       
    rclcpp::Subscription<motor_control_interfaces::msg::MotorData>::SharedPtr motor_sc;

    rclcpp::Publisher<std_msgs::msg::Int8>::SharedPtr sc_to_cs;
    rclcpp::Publisher<motor_control_interfaces::msg::MotorCommand>::SharedPtr sc_to_motor;

    rclcpp::TimerBase::SharedPtr publish_motor_command1;

    // ROS node initialization
    SC_FSM_Drill() : Node("SC_Drill") {

      std::cout << "Starting Drill" << std::endl;
      
      // --------------------------------------------------------------
      // SUBSCRIBERS

      // subscribe to instructions from the CS
      fsm_sc = this->create_subscription<std_msgs::msg::Int8>(
        "ROVER/SC_fsm", 
        1, 
        std::bind(&SC_FSM_Drill::fsm, this, _1)
      );  


      motor_sc = this->create_subscription<motor_control_interfaces::msg::MotorData>( 
        "motor_data", 
        10, 
        std::bind(&SC_FSM_Drill::motor_to_science, this, _1)
      );


      // ----------------------------------------------------------------
      // PUBLISHERS
      
      // publish feedback on FSM state to the CS
      sc_to_cs    = this->create_publisher<std_msgs::msg::Int8>("SC/fsm_state_to_cs", 10);
      sc_to_motor = this->create_publisher<motor_control_interfaces::msg::MotorCommand>("motor_command", 10);

      // Periodically publish motor commands
      publish_motor_command1 = this->create_wall_timer(100ms, std::bind(&SC_FSM_Drill::drive, this));

      // -----------------------------------------------------------------
      // DRILL INITIALISATION
      std::cout << "Listening" << std::endl;

    }

// =================================================================================

private:

  #define MOD1_SPEED 1000 
  #define MOD2_SPEED 1000
  #define DRILL_TORQUE 5

  #define MOTOR_MOD1 "SC_MOD1"
  #define MOTOR_MOD2 "SC_MOD2"
  #define MOTOR_DRILL "TURN_DRILL"

  #define VELOCITY_MODE 1
  #define TORQUE_MODE 2
  #define POSITION_MODE 0

  #define TARGET_POS_MOD1  -130000
  #define TARGET_POS_MOD2 125000

  #define DOWN 1
  #define UP -1

 
  // ######################################################################

  enum INSTRUCTION { LAUNCH = 1, ABORT = 2, WAIT = 3, RESUME = 4 };
  enum STATE       { IDLE = 1, MOD1 = 2, DRILL = 3, RETRACT = 4 };

  // After initialization, the drill is IDLE
  STATE current_state         = IDLE; 
  int abort_instruction       = 0;
  bool waiting                = false;
  int active                  = 0;

  double mod1_pos             = 0;
  double mod2_pos             = 0;

  int current_direction       = DOWN;

  motor_control_interfaces::msg::MotorCommand message = motor_control_interfaces::msg::MotorCommand();
  motor_control_interfaces::msg::MotorCommand message_drill = motor_control_interfaces::msg::MotorCommand();
  motor_control_interfaces::msg::MotorCommand message_abort = motor_control_interfaces::msg::MotorCommand();
  

  void drive(){
    if (active){
      std::cout << "ACTIVE" << std::endl;
      std::cout << message.name << std::endl;
      if (current_state == MOD1 || current_state == RETRACT){
        if((mod1_pos == TARGET_POS_MOD1 && current_direction == DOWN) || (mod1_pos == 0 && current_direction == UP)){
          active = 0;
        }else{
          this->sc_to_motor->publish(this->message);
        }
      }else if(current_state == DRILL){
        if((mod2_pos == TARGET_POS_MOD2 && current_direction == DOWN) || (mod2_pos == 0 && current_direction == UP)){
          active = 0;
        }else{
          this->sc_to_motor->publish(this->message);
        }
      }
      
    if (current_state == DRILL){
      this->sc_to_motor->publish(this->message_drill);
      }
    }
  }

  int mod1(int direction){
    
    std::cout << "MOD1 REACHED" << std::endl;
    current_direction = direction;
    message.name = MOTOR_MOD1;
    message.mode = POSITION_MODE;
    if (direction == DOWN) {
      message.commande = TARGET_POS_MOD1;
    } else {
      message.commande = 0;
    }
    // publish motor command
    
    this->active = 1;
  
    return 0;
  }

  int drill(int direction){

    current_direction = direction;
    message_drill.name = MOTOR_DRILL;
    message_drill.mode = TORQUE_MODE;
    if (direction == DOWN){
      message_drill.commande = DRILL_TORQUE;
    } else {
      message_drill.commande = 2*DRILL_TORQUE;
    }

    message.name = MOTOR_MOD2;
    message.mode = POSITION_MODE;
    if (direction == DOWN){
      message.commande = TARGET_POS_MOD2;
    } else {
      message.commande = 2000;
    }

    // publish motor command
    active = 1;

    return 0;
  }

  // Motor Data
  void motor_to_science(const motor_control_interfaces::msg::MotorData::SharedPtr msg){
    if (msg->name == MOTOR_MOD1){
      mod1_pos = msg->position;
    } else if (msg->name == MOTOR_MOD2){
      mod2_pos = msg->position;
    }
      
  }



  void notify_cs() {
    auto message_cs = std_msgs::msg::Int8();
    message_cs.data = current_state;
    // RCLCPP_INFO(this->get_logger(), "Publishing: '%d'", message_cs.data);
    sc_to_cs->publish(message_cs);
  }

  
  void fsm(const std_msgs::msg::Int8::SharedPtr msg){     
    
    RCLCPP_INFO(this->get_logger(), "Received: '%d'", msg->data);

    int instruction = msg->data;
    switch (instruction) {

      case static_cast<int>(INSTRUCTION::LAUNCH):
        next();
        break;

      case static_cast<int>(INSTRUCTION::ABORT):
        current_state = RETRACT;
        // abort(); 
        break;

      case static_cast<int>(INSTRUCTION::WAIT):
        waiting = true;
        active = 0;
        break;

      case static_cast<int>(INSTRUCTION::RESUME):              
        waiting = false;
        active = 1;
        break;

      default:
        RCLCPP_INFO(this->get_logger(), "Received: '%d'", msg->data);
        break;
    }
  }

  void next(){
    int ret = 0;
    switch(current_state){
      case static_cast<int>(STATE::IDLE):
        current_state = MOD1;
        std::cout << "IDLE TO MOD1" << std::endl;

        mod1(DOWN);
        
        notify_cs();

        break; 

      case static_cast<int>(STATE::MOD1):
        current_state = DRILL;
        std::cout << "MOD1 TO DRILL" << std::endl;
        drill(DOWN);
        
        notify_cs();
        
        break;

      case static_cast<int>(STATE::DRILL):
        current_state = RETRACT;
        drill(UP);
        notify_cs();
        
        break;
      
      case static_cast<int>(STATE::RETRACT):
        
        std::cout << "RETRACT TO IDLE" << std::endl;
        mod1(UP);
        
        current_direction = UP;
        message.name = MOTOR_MOD1;
        message.mode = POSITION_MODE;
        message.commande = 0;

        active = 1;
        
        break;

      default:
        break;
    }
  }

  
};

int main(int argc, char * argv[]) {
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<SC_FSM_Drill>());
  rclcpp::shutdown();
  return 0;
}


