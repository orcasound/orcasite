import { Box } from '@mui/material'
import Image from 'next/legacy/image'

export default function DetectionCategoryButton({
  icon,
  title,
}: {
  icon: { src: string }
  title: string
}) {
  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      <Image src={icon.src} alt={`${title} icon`} width={100} height={100} />
      {title}
    </Box>
  )
}
