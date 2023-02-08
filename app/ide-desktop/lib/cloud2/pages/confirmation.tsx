import { useRouter } from "next/router";

import { Auth } from "aws-amplify";
import { toast } from "react-hot-toast";

function Confirmation() {
  const router = useRouter();
  const verificationCode: string = router.query.verification_code;
  const email: string = router.query.email;

  if (email && verificationCode) {
    Auth.confirmSignUp(email, verificationCode)
      .then(() => router.push("/login?confirmed=true"))
      .catch(() =>
        toast.error(
          "Something went wrong! Please try again or contact the administrators."
        )
      );
  } else {
    router.push("/login?confirmed=false");
  }

  return;
}

Confirmation.isAuthComponent = true;
export default Confirmation;
