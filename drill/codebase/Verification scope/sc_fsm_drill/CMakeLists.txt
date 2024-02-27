cmake_minimum_required(VERSION 3.5)
project(sc_fsm_drill)

# Default to C99
if(NOT CMAKE_C_STANDARD)
  set(CMAKE_C_STANDARD 99)
endif()

# Default to C++14
if(NOT CMAKE_CXX_STANDARD)
  set(CMAKE_CXX_STANDARD 14)
endif()

if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  add_compile_options(-Wall -Wextra -Wpedantic)
endif()


# find dependencies
find_package(ament_cmake REQUIRED)
find_package(rclcpp REQUIRED)
find_package(std_msgs REQUIRED)
find_package(tutorial_interfaces REQUIRED)
find_package(sensor_msgs REQUIRED)
find_package(motor_control_interfaces REQUIRED)  
find_package(avionics_interfaces REQUIRED)

# uncomment the following section in order to fill in
# further dependencies manually.
#find_package(roscpp REQUIRED)
#find_package(rospy REQUIRED)


add_executable(talker src/publisher_member_function.cpp)
ament_target_dependencies(talker rclcpp tutorial_interfaces std_msgs motor_control_interfaces avionics_interfaces)  

#add_executable(motor_state src/motor_publisher.cpp)
#ament_target_dependencies(motor_state rclcpp tutorial_interfaces)  

add_executable(science_fsm src/simplepublisher.cpp)
#ament_target_dependencies(science_fsm rclcpp std_msgs)
ament_target_dependencies(science_fsm rclcpp tutorial_interfaces std_msgs sensor_msgs motor_control_interfaces avionics_interfaces) 

install(TARGETS
  talker
  science_fsm
  DESTINATION lib/${PROJECT_NAME})

ament_package()
