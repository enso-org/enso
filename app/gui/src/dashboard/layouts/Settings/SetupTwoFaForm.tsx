/**
 * @file
 *
 * 2FA Setup Settings Section. Allows users to setup, disable, and change their 2FA method.
 */
import ShieldCheck from '#/assets/shield_check.svg'
import ShieldCrossed from '#/assets/shield_crossed.svg'
import type { MfaType } from '#/authentication/cognito'
import {
  Alert,
  Button,
  ButtonGroup,
  CopyBlock,
  Dialog,
  DialogDismiss,
  DialogTrigger,
  Form,
  OTPInput,
  Selector,
  Switch,
  Text,
} from '#/components/AriaComponents'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useSessionAPI } from '#/providers/SessionProvider'
import { useText } from '#/providers/TextProvider'
import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import { lazy } from 'react'

const LazyQRCode = lazy(() =>
  // eslint-disable-next-line @typescript-eslint/naming-convention
  import('qrcode.react').then(({ QRCodeCanvas }) => ({ default: QRCodeCanvas })),
)

/**
 * 2FA Setup Settings Section.
 *
 * Allows users to setup, disable, and change their 2FA method.
 */
export function SetupTwoFaForm() {
  const { getText } = useText()
  const { getMFAPreference, updateMFAPreference, verifyTotpToken } = useSessionAPI()

  const { data } = useSuspenseQuery({
    queryKey: ['twoFaPreference'],
    queryFn: () => getMFAPreference(),
  })

  const MFAEnabled = data !== 'NOMFA'

  const updateMFAPreferenceMutation = useMutation({
    mutationFn: (preference: MfaType) => updateMFAPreference(preference),
    meta: { invalidates: [['twoFaPreference']] },
  })

  if (MFAEnabled) {
    return (
      <div className="flex w-full flex-col gap-4">
        <Alert variant="neutral" icon={ShieldCheck}>
          <Text.Group>
            <Text variant="subtitle" weight="bold">
              {getText('2FAEnabled')}
            </Text>
            <Text>{getText('2FAEnabledDescription')}</Text>
          </Text.Group>
        </Alert>

        <div className="flex w-full flex-col">
          <Text variant="subtitle" weight="bold">
            {getText('disable2FA')}
          </Text>

          <Text color="disabled" className="mb-4">
            {getText('disable2FADescription')}
          </Text>

          <DialogTrigger>
            <Button variant="delete" className="self-start" icon={ShieldCrossed}>
              {getText('disable2FA')}
            </Button>

            <Dialog title={getText('disable2FA')}>
              <Form
                /* eslint-disable-next-line @typescript-eslint/no-magic-numbers */
                schema={(z) => z.object({ otp: z.string().min(6).max(6) })}
                formOptions={{ mode: 'onSubmit' }}
                method="dialog"
                onSubmit={({ otp }) =>
                  verifyTotpToken(otp).then((passed) => {
                    if (passed) {
                      return updateMFAPreferenceMutation.mutateAsync('NOMFA')
                    } else {
                      throw new Error('Invalid OTP')
                    }
                  })
                }
              >
                <Text>{getText('disable2FAWarning')}</Text>

                <OTPInput autoFocus name="otp" maxLength={6} label={getText('verificationCode')} />

                <ButtonGroup>
                  <Form.Submit variant="delete">{getText('disable')}</Form.Submit>
                  <DialogDismiss />
                </ButtonGroup>

                <Form.FormError />
              </Form>
            </Dialog>
          </DialogTrigger>
        </div>
      </div>
    )
  }
  return (
    <Form
      schema={(z) =>
        z.object({
          enabled: z.boolean(),
          display: z.string(),
          /* eslint-disable-next-line @typescript-eslint/no-magic-numbers */
          otp: z.string().min(6).max(6),
        })
      }
      defaultValues={{ enabled: false, display: 'QR' }}
      onSubmit={async ({ enabled, otp }) => {
        if (enabled) {
          return verifyTotpToken(otp).then((passed) => {
            if (passed) {
              return updateMFAPreferenceMutation.mutateAsync('TOTP')
            } else {
              throw new Error('Invalid OTP')
            }
          })
        }
      }}
    >
      <>
        <Switch
          name="enabled"
          description={getText('enable2FADescription')}
          label={getText('enable2FA')}
        />

        <ErrorBoundary>
          <Suspense>
            <Form.FieldValue name="enabled">
              {(enabled) => enabled === true && <TwoFa />}
            </Form.FieldValue>
          </Suspense>
        </ErrorBoundary>
      </>
    </Form>
  )
}

/** Two Factor Authentication Setup Form. */
function TwoFa() {
  const { setupTOTP } = useSessionAPI()
  const { getText } = useText()

  const { data } = useSuspenseQuery({
    queryKey: ['setupTOTP'],
    queryFn: () => setupTOTP(),
  })

  return (
    <>
      <div className="flex w-full flex-col gap-4">
        <Selector name="display" items={['QR', 'Text']} aria-label={getText('display')} />

        <Form.FieldValue name="display">
          {(display) =>
            display === 'QR' && (
              <>
                <Alert key="alert" variant="neutral" icon={ShieldCheck}>
                  <Text.Group>
                    <Text variant="subtitle" weight="bold">
                      {getText('scanQR')}
                    </Text>

                    <Text>{getText('scanQRDescription')}</Text>
                  </Text.Group>
                </Alert>

                <div className="self-center">
                  <LazyQRCode
                    value={data.url}
                    bgColor="transparent"
                    fgColor="rgb(0 0 0 / 60%)"
                    size={192}
                    className="rounded-2xl border-0.5 border-primary p-4"
                  />
                </div>
              </>
            )
          }
        </Form.FieldValue>

        <Form.FieldValue name="display">
          {(display) =>
            display === 'Text' && (
              <>
                <Alert key="alert" variant="neutral" icon={ShieldCheck}>
                  <Text.Group>
                    <Text variant="subtitle" weight="bold">
                      {getText('copyLink')}
                    </Text>
                    <Text>{getText('copyLinkDescription')}</Text>
                  </Text.Group>
                </Alert>

                <CopyBlock copyText={data.url} />
              </>
            )
          }
        </Form.FieldValue>

        <OTPInput
          label={getText('verificationCode')}
          name="otp"
          maxLength={6}
          description={getText('verificationCodePlaceholder')}
        />
      </div>

      <ButtonGroup>
        <Form.Submit>{getText('enable')}</Form.Submit>

        <Form.Reset>{getText('cancel')}</Form.Reset>
      </ButtonGroup>

      <Form.FormError />
    </>
  )
}
