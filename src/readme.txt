# TO COMPILE:

arm-none-eabi-gcc NMPC.cpp AuxiliaryController.cpp -L/Users/maggiewang/Workspace/QuadcoptersWorkspace/NMPC_ARM/src -mfloat-abi=hard blas/*.o -lqpopt_c_static -lgcc -lc -lrdimon -lstdc++ -lm

# -mfloat-abi=hard is for compatibility with pixhawk px4