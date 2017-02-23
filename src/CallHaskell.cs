using System;
using System.Runtime.InteropServices;

namespace CharGen {
    class CallHaskell {
        [DllImport("libchargen", CallingConvention = CallingConvention.Cdecl)]
        private static extern void hs_init(IntPtr argc, IntPtr argv);

        [DllImport("libchargen", CallingConvention = CallingConvention.Cdecl)]
        private static extern void hs_exit();

        [DllImport("libchargen", CallingConvention = CallingConvention.Cdecl)]
        private static extern string c_newCharAsString(int seed);

        public static void Main(string[] args) {
            Console.WriteLine("Initializing runtime...");
            hs_init(IntPtr.Zero, IntPtr.Zero);

            try {
                Console.WriteLine("Calling to Haskell...");
                string readResult = Console.ReadLine();
                int i;
                int.TryParse(readResult, out i);
                string result = c_newCharAsString(i);
                Console.WriteLine(result);
            } finally {
                Console.WriteLine("Exiting runtime...");
                hs_exit();
            }
        }
    }
}
