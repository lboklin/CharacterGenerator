using System;
using System.Runtime.InteropServices;

namespace CharGen {
    class CallHaskell {
        [DllImport("libchargen.so", CallingConvention = CallingConvention.Cdecl)]
        private static extern void hs_init(IntPtr argc, IntPtr argv);

        [DllImport("libchargen.so", CallingConvention = CallingConvention.Cdecl)]
        private static extern void hs_exit();

        [DllImport("libchargen.so", CallingConvention = CallingConvention.Cdecl)]
        private static extern string c_newCharAsString(int seed);

        public static void Main(string[] args) {
            Console.WriteLine("Initializing runtime...");
            hs_init(IntPtr.Zero, IntPtr.Zero);

            try {
                Console.WriteLine("Calling to Haskell...");
                int readResult = Console.ReadLine();
                string result = c_newCharAsString(readResult);
                Console.WriteLine(result);
            } finally {
                Console.WriteLine("Exiting runtime...");
                hs_exit();
            }
        }
    }
}
