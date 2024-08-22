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
  DialogTrigger,
  Form,
  OtpInput,
  Selector,
  Switch,
  Text,
} from '#/components/AriaComponents'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useAuth, useFullUserSession } from '#/providers/AuthProvider'
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
  const { cognito } = useAuth()

  const { data } = useSuspenseQuery({
    queryKey: ['twoFa'],
    queryFn: () =>
      cognito.getMFAPreference().then((res) => {
        if (res.err) {
          throw res.val
        } else {
          return res.unwrap()
        }
      }),
  })

  const MFAEnabled = data !== 'NOMFA'

  const updateMFAPreferenceMutation = useMutation({
    mutationFn: (preference: MfaType) => cognito.updateMFAPreference(preference),
    meta: { invalidates: [['twoFa']] },
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
                schema={(z) =>
                  z.object({
                    otp: z
                      .string()
                      .min(6)
                      .max(6)
                      .refine(async (value) =>
                        cognito.verifyTotpToken(value).then((res) => res.ok),
                      ),
                  })
                }
                formOptions={{ mode: 'onSubmit' }}
                method="dialog"
                onSubmit={() => updateMFAPreferenceMutation.mutateAsync('NOMFA')}
              >
                <Text>{getText('disable2FAWarning')}</Text>

                <OtpInput name="otp" maxLength={6} label={getText('verificationCode')} />

                <ButtonGroup>
                  <Form.Submit variant="delete">{getText('disable')}</Form.Submit>
                  <Form.Submit formnovalidate>{getText('cancel')}</Form.Submit>
                </ButtonGroup>
              </Form>
            </Dialog>
          </DialogTrigger>
        </div>
      </div>
    )
  } else {
    return (
      <Form
        schema={(z) =>
          z.object({
            enabled: z.boolean(),
            display: z.string(),
            otp: z
              .string()
              .refine(async (value) => cognito.verifyTotpToken(value).then((res) => res.ok)),
          })
        }
        formOptions={{ mode: 'onSubmit' }}
        defaultValues={{ enabled: false, display: 'qr' }}
        onSubmit={async ({ enabled }) => {
          if (enabled) {
            await updateMFAPreferenceMutation.mutateAsync('TOTP')
          }
        }}
      >
        {({ values }) => (
          <>
            <Switch
              name="enabled"
              description={getText('enable2FADescription')}
              label={getText('enable2FA')}
            />

            <ErrorBoundary>
              <Suspense>{values.enabled === true && <TwoFa />}</Suspense>
            </ErrorBoundary>
          </>
        )}
      </Form>
    )
  }
}

/**
 * Two Factor Authentication Setup Form.
 */
function TwoFa() {
  const { cognito } = useAuth()
  const { email } = useFullUserSession()
  const { getText } = useText()

  const { data } = useSuspenseQuery({
    queryKey: ['setupTOTP'],
    queryFn: () =>
      cognito.setupTOTP().then((res) => {
        if (res.err) {
          throw res.val
        } else {
          return res.unwrap()
        }
      }),
  })

  const { field } = Form.useField({ name: 'display' })

  const str = 'otpauth://totp/AWSCognito:' + email + '?secret=' + data + '&issuer=' + 'Enso'

  return (
    <>
      <div className="flex w-full flex-col gap-4">
        <Selector name="display" items={['qr', 'text']} />

        {field.value === 'qr' && (
          <>
            <Alert variant="neutral">
              <Text.Group>
                <Text variant="subtitle" weight="bold">
                  {getText('scanQR')}
                </Text>

                <Text>{getText('scanQRDescription')}</Text>
              </Text.Group>
            </Alert>

            <div className="self-center">
              <LazyQRCode
                value={str}
                bgColor="transparent"
                fgColor="rgb(0 0 0 / 60%)"
                size={192}
                className="rounded-2xl border-0.5 border-primary p-4"
              />
            </div>
          </>
        )}

        {field.value === 'text' && (
          <>
            <Alert variant="neutral">
              <Text.Group>
                <Text variant="subtitle" weight="bold">
                  {getText('copyLink')}
                </Text>
                <Text>{getText('copyLinkDescription')}</Text>
              </Text.Group>
            </Alert>

            <CopyBlock copyText={str} />
          </>
        )}

        <OtpInput
          className="max-w-96"
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
